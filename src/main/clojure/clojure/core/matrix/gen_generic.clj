(ns clojure.core.matrix.gen-generic
  (:require [clojure.string :as s])
  (:require [clojure.walk :as walk])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set])
  (:require [clojure.core.matrix :as cm])
  (:require [clojure.core.matrix.protocols :as mp])
  (:import [java.io LineNumberReader InputStreamReader PushbackReader]))

(defn get-code
  "gets the code for the symbol x"
  [x]
  (when-let [v (resolve x)]
    (let [nspc (str (ns-name (:ns (meta v))))
          filename (str (s/replace (s/replace nspc "." "/" ) "-" "_") ".clj")]
      (when-let [strm (io/input-stream (io/resource filename))]
        (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
          (dotimes [_ (dec (:line (meta v)))]
            (.readLine rdr))
          (read (PushbackReader. rdr))
          )))))

(defn get-forms
  "returns a list of the sexps in the file"
  [file]
  (let [strm (io/input-stream (io/resource file))
        pbr (PushbackReader. (LineNumberReader. (InputStreamReader. strm)))]
    (loop [sexps []]
      (if-let [sexp (read pbr false nil)]
        (recur (conj sexps sexp)) sexps))))

(defn get-arities [dfn]
  (prn "get-arities for " dfn)
  (let [idx (if (string? (nth dfn 2)) 3 2)
        arities (drop idx dfn)
        _ (prn "dropped " arities (list? (first arities)))]
    (let [res (if-not (vector? (first arities)) arities (list arities))]
      (prn "results " res)
      res)))

(defn add-spec-to-argument-list [arity]
  (list* (vec (cons 'spec (first arity))) (rest arity)))

(defn append-spec-to-argument-list [arity]
  (list* (vec (conj (first arity) 'spec)) (rest arity)))

(defn replace-protocol-function-calls
  ([arity] (replace-protocol-function-calls arity nil))
  ([arity protocols]
     (walk/postwalk #(if (and (list? %) (symbol? (first %))
                              (.startsWith ^String (str (first %)) "mp/")
                              (or (nil? protocols)
                                  (some #{(resolve (first %))} protocols)))
                       (list* (symbol (str "gmp/generic-" (name (first %))))
                              (concat (rest %) ['spec]))
                       %) arity)))

(defn to-defn [code arities]
  (let [idx (if (string? (nth code 2)) 3 2)]
    (list* (concat (take idx code) arities))))

(def num-to-gen
  {1.0 '(:one spec)
   1 '(:one spec)
   0 '(:zero spec)
   0.0 '(:zero spec)
   -1 '((:sub spec) (:one spec))
   -1.0 '((:sub spec) (:one spec))
   '* '(:mul spec)
   '+ '(:add spec)
   '- '(:sub spec)
   '/ '(:div spec)
   '< '(:< spec)
   '> '(:> spec)
   '= '(:= spec)
   '>= '(:>= spec)
   '<= '(:<= spec)
   'Math/abs '(:abs spec)
   'Math/sqrt '(:sqrt spec)
   'number? '(:scalar? spec)
   `* '(:mul spec)
   `+ '(:add spec)
   `- '(:sub spec)
   `/ '(:div spec)
   `number? '(:scalar? spec)
   `< '(:< spec)
   `> '(:> spec)
   `= '(:= spec)
   `>= '(:>= spec)
   `<= '(:<= spec)})

(defn replace-with-spec-content [code]
  (let [to-replace (set (filter (set (keys num-to-gen)) (flatten code)))
        gensym-bindings (mapv (fn [s] [(gensym) s]) to-replace)
        gensym-map (into {} (map (comp vec reverse) gensym-bindings))
        ]
    `(let ~(vec (mapcat (fn [[gs s]] [gs (get num-to-gen s)]) gensym-bindings))
       ~@(walk/postwalk-replace gensym-map code))))

(defn replace-spec-content-arity [arity]
  `(~(first arity)
    ~(replace-with-spec-content (rest arity))))



(defn generify-code [code]
  (->> code
      get-arities
      (map add-spec-to-argument-list)
      (map replace-protocol-function-calls)
      (map replace-spec-content-arity)
      (to-defn code)
      ))

(defn create-generic-code [list-of-functions]
  (map (comp generify-code get-code) list-of-functions))

(defn get-protocol-functions [arity]
  (let [protocol-functions (atom #{})]
    (walk/postwalk #(do (when (and (symbol? %)
                                   (.startsWith ^String (str %) "mp/"))
                          (swap! protocol-functions set/union #{%}))
                        %) arity)
    (prn "arity " arity "protocol-functions " @protocol-functions)
    @protocol-functions))

(defn generic-protocol [protocol]
  (symbol
   (str "PGeneric"
        (.substring ^String (str (:name (meta protocol))) 1))))

(defn generate-generic-protocol-functions [proto-groups]
  (prn "proto-groups " proto-groups)
  (for [[protocol functions] proto-groups]
    `(~'defprotocol ~(generic-protocol protocol)
       ~@(map #(concat [(symbol (str "generic-" (:name (meta %))))]
                       (map (fn [args]
                              (conj args 'spec))
                            (:arglists (meta %)))) functions))))

(defn group-by-protocols [protocol-functions]
  (->> protocol-functions
       (map resolve)
       (group-by (comp :protocol meta))))
(defn dbg [x]
  (prn "dbg " x) x)

(defn get-protocols [list-of-functions]
  (->> (map #(->> %
                  get-code
                  get-arities
                  (map get-protocol-functions)
                  (apply set/union))
            list-of-functions)
       (apply set/union)
       group-by-protocols))

(defn create-generic-protocols [list-of-functions]
  (->> list-of-functions
       get-protocols
       generate-generic-protocol-functions))

(defn get-extend-protocols [protocols]
  (let [forms (get-forms "clojure/core/matrix/impl/default.clj")]
    (prn "protocols " protocols)
    (map #(some
           (fn [v] (and (= (list 'extend-protocol
                                 (symbol (str "mp/" (:name (meta %)))))
                           (take 2 v)) v)) forms) protocols)))

(defn spit-imp-map [imp-map protocols]
  (apply concat
         (for [[type methods] imp-map]
           (cons type
                 (map
                  (fn [method]
                    `(~(symbol (str "generic-" (first method)))
                      ~@(map (fn [arity]
                               (prn "arity " arity)
                               (-> arity
                                   append-spec-to-argument-list
                                   replace-spec-content-arity
                                   (replace-protocol-function-calls protocols)))
                             (get-arities (cons 'defn method))))) methods)))))

(defn test-for-scalar [obj-methods numb-methods]
  (for [obj-method obj-methods]
    (if-let [numb-method (some #(and (= (first obj-method)
                                        (first %))
                                     %) numb-methods)]
      (let [obj-method-arities (get-arities (cons 'defn obj-method))
            numb-method-arities (get-arities (cons 'defn numb-method))
            ]
        (list* (first obj-method)
              (map (fn [obj-ar numb-ar]
                     `(~(first obj-ar)
                       (if (~'number? ~(first (first obj-ar)))
                         (do ~@(rest numb-ar))
                         (do ~@(rest obj-ar)))))
                   obj-method-arities numb-method-arities)))
      obj-method)))

(defn handle-generic-scalar [imp-map]
  (let [obj (get imp-map 'Object)
        numb (get imp-map 'Number)
        _ (prn "old-obj " obj
               "new-obj" (test-for-scalar obj numb))]
    (if numb
      (-> imp-map
          (dissoc 'Number)
          (assoc 'Object (test-for-scalar obj numb))))))

(defn generify-extend-protocol [protocol-code protocols]
  (prn "protocol-code " protocol-code)
  (let [imp-map (->> protocol-code
                     (drop 2)
                     (partition-by #(or (symbol? %) (nil? %)))
                     (partition 2)
                     (map (fn [[l r]] [(first l) r]))
                     (into {}))
        imp-map (handle-generic-scalar imp-map)
        ]
    (prn "imp-map before" imp-map "imp-map-after" (spit-imp-map imp-map protocols))
    `(~'extend-protocol ~(symbol (str "gmp/PGeneric"
                                      (.substring ^String
                                                  (str (second protocol-code))
                                                  4)))
       ~@(spit-imp-map imp-map protocols))))


(defn create-generic-protocol-default-implementations [list-of-functions]
  (let [protocols (->> list-of-functions get-protocols vals (apply concat))]
    (->> list-of-functions
         get-protocols
         keys
         get-extend-protocols
         (map #(generify-extend-protocol % protocols)))))


;;TODO now able to write the generi-protocols and generic-default implementations
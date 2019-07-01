(ns libra.macros
  (:require [libra.free :refer [bind
                                free
                                pure
                                liftf
                                make-generic-interpreter
                                generic-get-state]]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(defmacro fdo [& statements]
  (let [statements (remove #(= 'let %) statements)
        [standard guarded] (split-with #(not (= % '-->)) statements)
        needs-guard? (> (count guarded) 1)
        with-guard (reduce
                    (fn [statements curr]
                      (if (and (= (count statements) (dec (count standard)))
                               needs-guard?)
                        (conj statements
                              `(if ~curr
                                 ~(nth guarded 1)
                                 (fdo ~@(drop 2 guarded))))
                        (conj statements curr)))
                    []
                    standard)]
    (letfn [(make-fdo [statements]
              (let [[curr & statements] statements
                    [arrow monad & other-statements] statements]
                (cond
                  (nil? statements) curr
                  (vector? curr) (let [add-rec-group (fn [forms form rec-group]
                                                       (concat forms
                                                               `(~(gensym) (do ~@(map (fn [[f a]] `(reset! ~a ~f))
                                                                                       rec-group)))
                                                               (if (nil? form) [] [form])))
                                       {:keys [sym-map forms rec-group]} (reduce
                                                                          (fn [agg form]
                                                                            (let [sym-name (if (symbol? form) (name form) "")
                                                                                  {:keys [name? sym-map rec-group forms]} agg
                                                                                  is-rec-name (and name?
                                                                                                   (string/starts-with? sym-name "f#"))
                                                                                  needs-resets (and name?
                                                                                                    (> (count rec-group) 0)
                                                                                                    (not is-rec-name))
                                                                                  f-sym (if is-rec-name (gensym sym-name) nil)
                                                                                  a-sym (if is-rec-name (gensym sym-name) nil)]
                                                                              {:name? (not name?)
                                                                               :sym-map (if is-rec-name
                                                                                          (merge sym-map {form `(deref ~a-sym)})
                                                                                          sym-map)
                                                                               :rec-group (cond
                                                                                            (not name?) rec-group
                                                                                            is-rec-name (conj rec-group [f-sym a-sym])
                                                                                            :else nil)
                                                                               :forms (cond
                                                                                        is-rec-name (concat forms
                                                                                                            `[~a-sym (atom nil)
                                                                                                              ~f-sym])
                                                                                        needs-resets (add-rec-group forms form rec-group)
                                                                                        :else (concat forms [form]))}))
                                                                          {:name? true :forms []}
                                                                          curr)
                                       forms (add-rec-group forms nil rec-group)
                                       forms (walk/postwalk #(get sym-map % %) forms)
                                       statements (walk/postwalk #(get sym-map % %) statements)]
                                   `(let [~@forms] ~(make-fdo statements)))
                  (= arrow (symbol '<-)) `(bind ~monad (fn [~curr] ~(make-fdo other-statements)))
                  :else `(bind ~curr (fn [~(gensym)] ~(make-fdo statements))))))]
      (make-fdo with-guard))))

(defmacro deffree [name funcs]
  (let [sym (gensym name)
        int-name (symbol (str "make-interpreter"))
        state-name (symbol (str "get-state"))
        makes (mapcat (fn [func]
                        (let [ref-sym (gensym (.toString func))
                              id-sym (symbol (str "id-" (.toString func)))
                              make-sym (symbol (str "make-" (.toString func)))
                              ]
                          [`(def ~id-sym '~ref-sym)
                           `(defn ~make-sym [f# source# & args#]
                              {:args args#
                               :f f#
                               :free '~sym
                               :source source#
                               :fmap (fn [g#] (apply ~make-sym (fn [v#] (-> v# f# g#)) source# args#))}
                              )])) funcs)
        fs (map (fn [func]
                  (let [make-sym (symbol (str "make-" (.toString func)))
                        f-sym (symbol (.toString func))]
                    `(defn ~f-sym [& args#] (liftf (apply ~make-sym (fn [a#] a#) ~f-sym args#))))) funcs)]
    `(do
       (def ~name '~sym)
       (def ~int-name (fn [& args#] (apply (make-generic-interpreter '~sym) args#)))
       (def ~state-name (fn [i-res#] (generic-get-state i-res# '~sym)))
       ~@makes
       ~@fs)))

(defn add-fdo [sym]
  (fn [& statements]
    (let [fn? (= sym 'fn)
          defn? (= sym 'defn)
          n? (or defn? fn?)
          docstring? (and (not fn?) (string? (nth statements 1)))
          take-n (+ 1 (if docstring? 1 0) (if fn? -1 0))
          untouched (take take-n statements)
          add-to-fn (fn [[args & statements]]
                      `(~args (fdo ~@statements)))
          rest (drop take-n statements)
          updated (cond
                    (and n? (vector? (first rest))) (add-to-fn rest)
                    n? (map add-to-fn rest)
                    :else `((fdo ~@rest)))]
      `(~sym ~@untouched ~@updated))))

(defn parse-int [s default]
  (try
    #?(:cljs (js/parseInt s 10)
       :clj (Integer/parseInt s))
    #?(:cljs (catch js/Object e default)
       :clj (catch Exception e default))))

(defmacro ffn [& statements]
  (apply (add-fdo 'fn) statements))

(defn to-safe-free [freef]
  (let [free (atom (freef))]
    (if (not (:bound? @free)) @free
        {:safe-free? true
         :bind (fn [& args]
                 (let [res (apply bind @free args)]
                   (reset! free (freef))
                   res))})))
(defmacro deff [name & statements]
  `(def ~name
     (to-safe-free (ffn [] ~@statements))))

(defmacro deffn [& statements]
  (apply (add-fdo 'defn) statements))
(defmacro whenf [bool & statements]
  `(if ~bool (fdo ~@statements) (pure nil)))
(defmacro varg# [statement]
  (let [args (gensym "args")]
    `(fn [& ~args]
       ~(->> [statement]
             (walk/postwalk (fn [sym]
                              (let [sym-name (if (symbol? sym) (name sym) "")]
                                (if (string/starts-with? sym-name "%")
                                  `(nth ~args ~(-> sym-name (subs 1) (parse-int -1) dec) nil)
                                  sym))))
             first))))

(ns libra.macros
  (:require [libra.free :refer [bind
                                free
                                pure
                                return
                                liftf
                                make-generic-interpreter
                                generic-get-state]]))

(defmacro fdo [& statements]
  (letfn [(make-fdo [statements]
            (let [[curr & statements] statements
                  [arrow monad & other-statements] statements]
              (cond
                (nil? statements) curr
                (vector? curr) `(let ~curr ~(make-fdo statements))
                (= arrow (symbol '<-)) `(bind ~monad (fn [~curr] ~(make-fdo other-statements)))
                :else `(bind ~curr (fn [~(gensym)] ~(make-fdo statements))))))]
    (make-fdo statements)))

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

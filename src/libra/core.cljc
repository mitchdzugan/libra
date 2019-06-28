(ns libra.core
  (:require [libra.free :as lfree]
            [libra.interpret :as linterpret]
            [libra.inject :as inject]
            [libra.macros :as macros]))

(def bind lfree/bind)
(def free lfree/free)
(def pure lfree/pure)
(defn return
  ([] (lfree/pure nil))
  ([v] (lfree/pure v)))
(def liftf lfree/liftf)
(def combine-interpreters linterpret/combine-interpreters)
(def get-res linterpret/get-res)
(def interpret linterpret/interpret)
(def run linterpret/run)
(def interpret-with-overrides inject/interpret-with-overrides)
(defn run-with-overrides [overrides m]
  (bind (interpret-with-overrides overrides m) #(return (get-res %))))
(defn of-list [[mx & mxs]]
  (if mx
    (macros/fdo
     x <- mx
     xs <- (of-list mxs)
     (return (conj xs x)))
    (return [])))
(defmacro varg# [& statements] `(macros/varg# ~@statements))
(defmacro whenf [& statements] `(macros/whenf ~@statements))
(defmacro deffree [& statements] `(macros/deffree ~@statements))
(defmacro deffn [& statements] `(macros/deffn ~@statements))
(defmacro deff [& statements] `(macros/deff ~@statements))
(defmacro ffn [& statements] `(macros/ffn ~@statements))
(defmacro fdo [& statements] `(macros/fdo ~@statements))
(defn reader [from-state]
  (fn [{:keys [next state]}] (next (apply from-state state))))
(defn updater [from-state-and-args]
  (fn [{:keys [next state args]}]
    (next nil (apply from-state-and-args state args))))
(defn writer [field]
  (updater #(update %1 field conj %2)))
(defn setter [field-from-args value-from-args]
  (updater (fn [state & args]
             (assoc state
                    (apply field-from-args args)
                    (apply value-from-args args)))))
(defn setter-in [fields-from-args value-from-args]
  (updater (fn [state & args]
             (println args)
             (assoc-in state
                       (apply fields-from-args args)
                       (apply value-from-args args)))))

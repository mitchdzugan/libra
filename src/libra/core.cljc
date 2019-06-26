(ns libra.core
  (:require [libra.free :as lfree]
            [libra.interpret :as linterpret]
            [libra.inject :as inject]))

(def bind lfree/bind)
(def free lfree/free)
(def pure lfree/pure)
(def return lfree/return)
(def liftf lfree/liftf)
(def combine-interpreters linterpret/combine-interpreters)
(def get-res linterpret/get-res)
(def interpret linterpret/interpret)
(def run linterpret/run)
(def interpret-with-overrides inject/interpret-with-overrides)
(defn run-with-overrides [overrides m]
  (bind (interpret-with-overrides overrides m) #(return (get-res %))))

(ns libra.interpret
  (:require [libra.free :refer [pure free return bind]]
            [libra.inject :refer [make-inject]]
            [libra.utils :as utils]))

(defn interpret-r [interpreter {:keys [pure? value bound?] :as freem}]
  (if pure? value
      (let [{:keys [get-bound main]} (if bound? freem (bind freem pure))
            bound (get-bound)
            {:keys [cont free]} (main)
            {{:keys [value]} :value} ((:process interpreter) cont)
            next (free value)
            res (interpret-r interpreter (:cont next))]
        (reduce (fn [res fm] (interpret-r interpreter (fm res))) res bound))))

(defn combine-interpreters [& is]
  (let [i-by-sym (utils/index-by :symbol is)]
    {:get-state (fn [sym]
                  (let [i (get i-by-sym sym)]
                    ((:get-state i) sym)))
     :process (fn [{:keys [free] :as m}]
                (let [i (get i-by-sym free)]
                  ((:process i) m)))}))
(def get-res :res)
(defn interpret [& args]
  (let [[freem & interpreters] (reverse args)
        interpreter (apply combine-interpreters
                           (make-inject interpret interpreters)
                           interpreters)
        res (interpret-r interpreter freem)]
    {:res res :interpreter interpreter}))
(defn run [& args]
  (get-res (apply interpret args)))

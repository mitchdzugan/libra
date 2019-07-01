(ns libra.interpret
  (:require [libra.free :refer [pure free bind]]
            [libra.inject :refer [make-inject]]
            [libra.utils :as utils]))

(defn combine-interpreters [& is]
  (let [i-by-sym (utils/index-by :symbol is)]
    {:get-state (fn [sym]
                  (let [i (get i-by-sym sym)]
                    ((:get-state i) sym)))
     :process (fn [{:keys [free] :as m} & args]
                (let [i (get i-by-sym free)]
                  (apply (:process i) m args)))}))
(def get-res :res)
(defn interpret [& args]
  (let [[freem & interpreters] (reverse args)
        interpreter (apply combine-interpreters
                           (make-inject interpret interpreters)
                           interpreters)]
    (letfn [(itpt-r [{:keys [pure? bound? value] :as freem}]
              (if pure? value
                  (let [{:keys [get-bound main]} (if bound? freem (bind freem pure))
                        bound (get-bound)
                        {:keys [cont free] :as oh} (main)
                        value (if (:skip-interpret? cont)
                                (:v cont)
                                (get-in ((:process interpreter) cont #(apply interpret (concat interpreters %&)))
                                        [:value :value]))
                        next (free value)
                        res (itpt-r (:cont next))]
                    (reduce (fn [res fm] (itpt-r (fm res))) res bound))))]
      {:res (itpt-r freem) :interpreter interpreter})))

(defn run [& args]
  (get-res (apply interpret args)))

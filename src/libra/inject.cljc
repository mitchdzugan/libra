(ns libra.inject
  (:require [libra.utils :as utils]
            #?(:clj [libra.macros :refer [deffree fdo]]
               :cljs [libra.macros :refer-macros [deffree fdo]])))

(deffree inject [interpret-with-overrides])

(defn make-inject [interpret interpreters]
  (make-interpreter
   {interpret-with-overrides (fn [{:keys [next] [overrides m] :args}]
                               (-> interpret
                                   (apply interpret (concat
                                                     (->> overrides
                                                          (utils/index-by :symbol)
                                                          (merge (->> interpreters
                                                                      (utils/index-by :symbol)))
                                                          vals)
                                                     [m]))
                                   next))}))

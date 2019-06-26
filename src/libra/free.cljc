(ns libra.free)

(defn bind [m fm] ((:bind m) fm))
(defn free [cont]
  {:cont cont
   :free? true
   :bind (fn [fm]
           (let [bound (atom [])
                 res-atom (atom nil)
                 res {:get-bound #(-> @bound)
                      :bound? true
                      :bind (fn [fm]
                              (swap! bound conj fm)
                              @res-atom)
                      :main (fn []
                              {:cont cont
                               :free (fn [v] (free (fm v)))})}]
             (reset! res-atom res)
             @res-atom))})

(defn pure [v]
  {:pure? true
   :value v
   :bind (fn [fm]
           (let [bound (atom [])
                 res-atom (atom nil)
                 res {:get-bound #(@bound)
                      :bound? true
                      :bind (fn [fm]
                              (swap! bound conj fm)
                              @res-atom)
                      :main (fn []
                              {:cont {}
                               :free (fn [] (free (fm v)))})}]
             (reset! res-atom res)
             @res-atom))})

(def return pure)
(defn liftf [command] (free ((:fmap command) pure)))

(defn make-generic-interpreter [sym]
  (fn [& args]
    (let [funcs (nth args 0 {})
          initial-state (nth args 1 nil)
          state (atom initial-state)
          process (fn [{:keys [args f source]}]
                    (let [next (fn [& args]
                                 (let [res (nth args 0 nil)
                                       new-state (nth args 1 @state)]
                                   (reset! state new-state)
                                   (-> res f pure)))]
                      ((get funcs source)
                       {:next next :state @state :args args})))]
      {:symbol sym :process process :get-state (fn [_] @state)})))

(defn generic-get-state [i-res sym]
  ((:get-state (:interpreter i-res)) sym))

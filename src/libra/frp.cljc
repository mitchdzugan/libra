(ns libra.frp
  (:require [beicon.core :as rx]
            #?(:clj  [libra.macros :refer [varg#]]
               :cljs [libra.macros :refer-macros [varg#]])))

(defn signal [a] {:type :signal :a a :sub (rx/from-atom a)})
(defn event [a] {:type :event :a a :sub (rx/from-atom a)})

(defn fmap-h [init creator f t]
  (let [ta (:a t)
        a (atom init)
        res (creator a)]
    (rx/on-value (:sub t) #(reset! a (f %)))
    res))

(defmulti fmap (varg# (-> %2 :type)))
(defmethod fmap :signal [f s] (fmap-h (f @(:a s)) signal f s))
(defmethod fmap :event [f e] (fmap-h nil event f e))

(defn bind-h [init-from-tt creator t ft]
  (let [tt (fmap ft t)
        a (atom (init-from-tt tt))
        res (creator a)]
    (rx/on-value (:sub tt)
                 (fn [t] (rx/on-value (:sub t) #(reset! a %))))
    res))
(defmulti bind (varg# (-> %1 :type)))
(defmethod bind :event [e fe] (bind-h (varg# nil) event e fe))
(defmethod bind :signal [s fs]
  (let [sfs (fn [s]
              (fs
               (if (= :signal (:type s))
                 s (signal (atom s)))))]
    (bind-h #(-> % :a deref :a deref) signal s sfs)))

(defmulti consume (varg# (-> %2 :type)))
(defmethod consume :signal [f s]
  (let [res (f @(:a s))]
    (rx/on-value (:sub s) f)
    res))
(defmethod consume :event [f e] (rx/on-value (:sub e) f))

(defmulti t-reduce (fn [& args] (-> args reverse first :type)))
(defmethod t-reduce :event [f init e]
  (let [a (atom init)
        res (signal a)]
    (rx/on-value (:sub e)
                 #(reset! a (f @a %)))
    res))
(defmethod t-reduce :signal [f init s]
  (let [a (atom (f init s))
        res (signal a)]
    (rx/on-value (:sub s)
                 #(reset! a (f @a %)))
    res))

(defn join [& es]
  (let [a (atom nil)
        res (event a)]
    (doseq [e es]
      (rx/on-value (:sub e) #(reset! a %)))
    res))

(defn of-map [m]
  (let [a (atom nil)
        res (event a)]
    (doseq [[k e] m]
      (rx/on-value (:sub e) #(reset! a [k %])))
    res))

(defn t-filter [p t]
  (let [a (atom nil)
        res (event a)]
    (rx/on-value (:sub t)
                 #(if (p %) (reset! a %) nil))
    res))

(defn zip-with [f & ss]
  (let [a (atom nil)
        calc! (fn [] (reset! a (apply f (map #(-> @(:a %)) ss))))]
    (calc!)
    (let [res (signal a)]
      (doseq [s ss]
        (rx/on-value (:sub s) calc!))
      res)))

(defn changed [s] (event (:a s)))


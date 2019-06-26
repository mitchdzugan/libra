(ns libra.utils)

(defn map-values [f m] (into {} (for [[k v] m] [k (f v)])))
(defn index-by [f l] (->> l (group-by f) (map-values first)))

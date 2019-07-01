(ns libra.dom
  (:require [libra.frp :as frp]
            [beicon.core :as rx]
            [libra.interpret :as libra]
            #?(:clj  [libra.macros :refer [deffree ffn]]
               :cljs [libra.macros :refer-macros [deffree ffn]])))

(deffree dom [; done
              get-env
              ; done
              get-signal
              ; done
              join-event
              ; done
              keyed
              ; done
              with-env
              ; done
              bind-signal
              ; done
              reduce-event
              ; done
              write-tag])

(defn ratom [init]
  #?(:cljs (reagent.core/atom init)
     :clj (atom init)))

(defn frag ([children] (frag children nil))
  ([children key]
   (fn [children key]
     (->> children
          reverse
          (concat (if key [:<> {:key key}] [:<>]))
          vec))))

(defn event-reduce [_ interpret reducer event init owner!]
  (let [val! (atom init)
        valS (frp/signal val!)
        prev-event! (atom nil)
        ires (interpret valS)
        {:keys [events els]} (get-state ires)]
    (reset! owner! {:events events
                    :res (libra/get-res ires)})
    (fn [_ interpret reducer event init owner!]
      (when (not (= event @prev-event!))
        (reset! prev-event! event)
        (frp/consume #(reset! val! (reducer @val! %)) event))
      [frag els])))

(defn signal-bind [_ interpret signal owner!]
  (let [render-count! (ratom 0)
        body! (atom nil)]
    (frp/consume #(let [ires (interpret %)
                        {:keys [els events]} (get-state ires)]
                    (reset! body! [frag els :sb-frag])
                    (reset! owner! {:events events
                                    :res (libra/get-res ires)})
                    (swap! render-count! inc))
                 signal)
    (fn [_ i s o] @render-count! @body!)))

(defn take-key [state type]
  (let [type-count (get-in state [:counts type] 0)
        keyed (get-in state [:env :keyed])]
    {:key (or keyed (str type "::" type-count))
     :state (-> state
                (assoc-in [:env :keyed] nil)
                (assoc-in [:counts type] (if keyed type-count (inc type-count))))}))

(defn interpreter [env-init]
  (make-interpreter {get-env
                     (fn [{:keys [next state]}]
                       (next (:env state)))

                     get-signal
                     (fn [{:keys [next state args]}]
                       (let [[s] args] (next (-> state :env :signals (get s)))))

                     join-event
                     (fn [{:keys [next state args]}]
                       (let [[k e] args]
                         (next nil (update-in state [:events k]
                                              #(if % (frp/join e %) e)))))

                     keyed
                     (fn [{:keys [next state args]}]
                       (let [[next-key] args]
                         (next nil (assoc-in state [:env :keyed] next-key))))

                     with-env
                     (fn [{:keys [next state args interpret-with-overrides]}]
                       (let [[env free] args
                             ires (interpret-with-overrides
                                   (interpreter env) free)
                             {:keys [els events]} (get-state ires)]
                         (next (-> libra/get-res ires)
                               (reduce-kv
                                (fn [s k e]
                                  (update-in s [:events k]
                                             #(if % (frp/join % e) e)))
                                (update state :els conj [frag els])
                                events))))

                     reduce-event
                     (fn [{:keys [next state args interpret-with-overrides]}]
                       (let [[event reducer init mps] args
                             is-sys? (not (= :event (:type event)))
                             sys (if is-sys? event nil)
                             ires! (atom nil)
                             iresS (frp/signal ires!)
                             event (if is-sys?
                                     (frp/bind (frp/changed iresS)
                                               #(or (get-in % [:events sys])
                                                    (frp/event (atom nil))))
                                     event)
                             env (:env state)
                             events (:events state)
                             events (if is-sys? (dissoc events sys) events)
                             ps (if (not is-sys?) mps
                                    (ffn [sig]
                                         env <- (get-env)
                                         (with-env (assoc-in env [:signals sys] sig) mps)))
                             child-env (if is-sys? (update env :init-events conj sys) env)
                             child-interpret #(interpret-with-overrides (interpreter child-env) (ps %))
                             {:keys [key state]} (take-key state :reduce-event)
                             el [event-reduce {:key key} child-interpret reducer event init ires!]]
                         (next (frp/fmap :res iresS)
                               (reduce-kv
                                (fn [s k e]
                                  (let [childE (frp/bind (frp/changed iresS)
                                                         #(or (get-in % [:events k])
                                                              (frp/event (atom nil))))
                                        joinedE (if e (frp/join e childE) childE)]
                                    (assoc-in s [:events k] joinedE)))
                                (-> state
                                    (update :els conj el)
                                    (assoc :events events))
                                events))))

                     bind-signal
                     (fn [{:keys [next state args interpret-with-overrides]}]
                       (let [[signal fs] args
                             signal (if (= :signal (:type signal))
                                      signal (get-in state [:env :signals signal]))
                             interpret #(interpret-with-overrides (interpreter (:env state)) (fs %))
                             ires! (atom nil)
                             iresS (frp/signal ires!)
                             {:keys [key state]} (take-key state :bind-signal)
                             el [signal-bind {:key key} interpret signal ires!]]
                         (next (frp/fmap :res iresS)
                               (reduce-kv
                                (fn [s k e]
                                  (let [childE (frp/bind (frp/changed iresS)
                                                         #(or (get-in % [:events k])
                                                              (frp/event (atom nil))))
                                        joinedE (if e (frp/join e childE) childE)]
                                    (assoc-in s [:events k] joinedE)))
                                (update state :els conj el)
                                (:events state)))))

                     write-tag
                     (fn [{:keys [next state args interpret-with-overrides]}]
                       (let [[tag u1 u2] args
                             env (:env state)
                             is-free? #(or (:pure? %)
                                           (:free? %)
                                           (:bound? %)
                                           (:safe-free? %))
                             has-attr? (and (map? u1)
                                            (not (is-free? u1)))
                             [attrs mfree] (if has-attr? [u1 u2] [u2 u1])
                             free? (is-free? mfree)

                             {:keys [inner-res events body]}
                             (if (not free?) {:body mfree}
                                 (let [ires (if free?
                                              (interpret-with-overrides
                                               (interpreter env) mfree)
                                              nil)
                                       {:keys [els events]} (get-state ires)]
                                   {:body [frag els]
                                    :events events
                                    :inner-res (libra/get-res ires)}))

                             ref! (atom nil)
                             refE (frp/event ref!)
                             onchange! (atom nil)
                             onchangeE (frp/event onchange!)
                             ons! (atom {"onchange" onchangeE})
                             mkons (fn [type]
                                     (let [make-tE
                                           (fn [ref]
                                             (let [t! (atom nil)
                                                   tE (frp/event t!)]
                                               (if ref (aset ref type #(reset! t! %))
                                                   nil)
                                               tE))]
                                       (or (get @ons! type)
                                           (let [res (frp/bind refE make-tE)]
                                             (swap! ons! merge {type res})
                                             res))))
                             el [tag (merge attrs {:onChange #(reset! onchange! %)
                                                   :ref #(reset! ref! %)}) body]]
                         (next {:mkons mkons :inner-res inner-res}
                               (reduce-kv
                                (fn [s k e]
                                  (update-in s [:events k]
                                             #(if % (frp/join % e) e)))
                                (update state :els conj el)
                                events))))}
                    {:env env-init
                     :events (reduce #(assoc %1 %2 nil) {} (:init-events env-init))}))

(defn div [& args] (apply write-tag :div args))
(defn span [& args] (apply write-tag :span args))
(defn p [& args] (apply write-tag :p args))
(defn a [& args] (apply write-tag :a args))
(defn button [& args] (apply write-tag :button args))
(defn input [& args] (apply write-tag :input args))

(defn onclick [{:keys [mkons]}] (mkons "onclick"))
(defn onchange [{:keys [mkons]}] (mkons "onchange"))
(defn onkeyup [{:keys [mkons]}] (mkons "onkeyup"))

(defn of-env-and-free [env free]
  [frag (-> env interpreter (libra/interpret free) get-state :els)])

(ns elon-tusk.engine
  (:require [fbc-utils.core :as ut]
            [snek.core :as sn]
            [clojure.pprint :as pp]))

;;;;;;;;;;;;; fbc-utils inline ;;;;;;;;;

(defn key-max [db]
  (apply max (keys db)))

(defn next-key [db]
  (if (seq db)
    (inc (key-max db))
    0))

(defn push-with-id [db obj]
  (let [id (next-key db)]
    (assoc db id (assoc obj :id id))))

(defn sqrt [x] (Math/sqrt x))
(defn square [x] (* x x))
(defn ceil [x] (Math/ceil x))

;; Logging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def log-atom (atom []))

(defn log [item]
  (swap! log-atom conj item))

(defn purge-log []
  (let [k @log-atom]
    (reset! log-atom [])
    k))

;; Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def foods {:mammoth {:id    :mammoth
                      :ease  4
                      :order 0}
            :ketchup {:id    :ketchup
                      :ease  8
                      :order 1}
            :lettuce {:id    :lettuce
                      :ease  6
                      :order 2}
            :bread   {:id    :bread
                      :ease  6
                      :order 3}})

(def food-num (count foods))
(def stonage-word-1 [:saber :rock :arrow :mammoth :fur :sling])
(def stonage-word-2 [:skull :head :chest :foot :nose :arms])
(def stonage-full-names [:elon-tusk :king-fred :yakyak :ogg :biggo :berf :snuud :yikyik :rokko :yukko])

(def max-production 10)
(def money-supply-per-agent 1000)
(def agents-num 40)

;; State initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-agent []
  {:money money-supply-per-agent})

(defn init-state []
  {:agents      (reduce push-with-id
                        {}
                        (map (fn [agent nam]
                               (assoc agent :name nam))
                             (repeatedly agents-num gen-agent)
                             (concat (map name stonage-full-names)
                                     (repeatedly (fn []
                                                   (str (name (rand-nth stonage-word-1)) "-" (name (rand-nth stonage-word-2))))))))
   :agent-foods (into {}
                      (for [agent-id (range agents-num)
                            food     (keys foods)]
                        [[agent-id food] {:id           [agent-id food]
                                          :productivity (int (rand-int (int (* 2 (get-in foods [food :ease])))))
                                          :owned        0}]))
   :orders      {}
   :foods       (into {}
                      (for [food (keys foods)]
                        [food {:id        food
                               :price     150
                               :order-cur 0
                               :order-num 0}]))})

(comment
  ((:agents (init-state)) 5) ; {:money 1000, :name "berf", :id 5}
  (first (:agent-foods (init-state))) ; [[39 :lettuce] {:id [39 :lettuce], :productivity 2, :owned 0}]
  (:ketchup (:foods (init-state))) ; {:id :ketchup, :price 150, :order-cur 0, :order-num 0}
  )

;; Burger making calculations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn target-formula [cost-others max-amount money max-price]
  (ceil (/ (- (sqrt (+ (* 4 max-amount cost-others max-price) (* 4 cost-others money) (square max-price))) max-price) (* 2 cost-others))))

(target-formula 20 100 500 20)

(defn target-burgers-optimum-formula ;;How many burgers can the agent expect to make, given owned food & money, assuming agent doesn't buy their most abundant food.
  [{:keys [foods]
    :as   state}
   agent-id]
  (let [ownership             (for [food (keys foods)]
                                [food (get-in state [:agent-foods [agent-id food] :owned])])
        [max-food max-amount] (apply max-key second ownership)
        max-price             (get-in state [:foods max-food :price])
        cost-others           (apply +
                                     (keep (fn [food]
                                             (when (not= food max-food)
                                               (get-in state [:foods food :price])))
                                           (keys foods)))
        starting-money        (get-in state [:agents agent-id :money])]
    (target-formula cost-others max-amount starting-money max-price)))

(defn calc-affordability [ownership money]
  (let [min-food-owned (:owned (apply min-key :owned ownership))]
    (loop [min-food-owned min-food-owned
           money          money]
      (let [next-min-food-owned (when-let [k (seq (filter (fn [{:keys [owned] :as food}]
                                                            (< min-food-owned owned))
                                                          ownership))]
                                  (apply min (map :owned k)))
            cost                (apply +
                                       (map :price
                                            (filter (fn [{:keys [owned] :as food}]
                                                      (>= min-food-owned owned))
                                                    ownership)))
            max-units           (int (/ money cost))
            max-level           (+ min-food-owned max-units)]
        (if (or (not next-min-food-owned) (>= next-min-food-owned max-level))
          max-level
          (recur next-min-food-owned (- money (* (- next-min-food-owned min-food-owned) cost))))))))

(defn target-burgers-as-much-as-affordable
  [{:keys [foods]
    :as   state}
   agent-id]
  (let [ownership (for [food-id (keys foods)]
                    {:food-id food-id
                     :owned   (get-in state [:agent-foods [agent-id food-id] :owned])
                     :price   (get-in state [:foods food-id :price])})
        money     (get-in state [:agents agent-id :money])]
    (calc-affordability ownership money)))

;; model loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn work [{:keys [agents]
             :as   state}
            {:keys [vacuum]
             :as options}]
  (reduce (fn [{:keys [foods]
                :as   acc}
               {:keys [id
                       name]
                :as   item}]
            (let [[food produced] (apply max-key
                                         (fn [[food productivity]]
                                           (* productivity (get-in foods [food :price])))
                                         (for [food (keys foods)]
                                           [food (if (and vacuum (= name "elon-tusk"))
                                                   (if (= food :ketchup)
                                                     150
                                                     1)
                                                   (get-in acc [:agent-foods [id food] :productivity]))]))
                  owned           (get-in acc [:agent-foods [id food] :owned])
                  amount          (+ owned produced)
                  acc             (assoc-in acc [:agent-foods [id food] :owned] amount)
                  burgers         (target-burgers-as-much-as-affordable acc id)
                  order-num       (get-in acc [:foods food :order-num])]
              (log {:cmd      :work
                    :agent-id id
                    :food     food
                    :produced produced})
              (-> acc
                  (assoc-in [:orders [food order-num]]
                            {:id       [food order-num]
                             :agent-id id
                             :amount   (max 0 (- amount burgers))})
                  (update-in [:foods food :order-num] inc))))
          state
          (shuffle (vals agents))))

(defn eat [{:keys [agents]
            :as   state}]
  (reduce (fn [acc {:keys [id] ;;iterate through agents
                    :as   item}]
            (let [burgers (target-burgers-as-much-as-affordable state id)
                  acc     (reduce (fn [acc2 item2] ;;iterate through foods
                                    (let [goal (max 0 (- burgers (get-in acc [:agent-foods [id item2] :owned])))]
                                      (loop [{:keys [foods
                                                     agents
                                                     orders]
                                              :as   acc2} acc2
                                             goal         goal] ;;iterate through orders
                                        (if (pos? goal)
                                          (let [{:keys [order-cur
                                                        order-num
                                                        price]
                                                 :as   food} (foods item2)]
                                            (if (not= order-cur order-num)
                                              (let [{:keys          [amount]
                                                     other-agent-id :agent-id
                                                     :as            order} (orders [item2 order-cur])]
                                                (let [n    (min amount goal (int (/ (get-in acc2 [:agents id :money]) price)))
                                                      k    (* price n)
                                                      acc2 (-> acc2
                                                               (update-in [:agents id :money] - k)
                                                               (update-in [:agents other-agent-id :money] + k)
                                                               (update-in [:agent-foods [id item2] :owned] + n)
                                                               (update-in [:agent-foods [other-agent-id item2] :owned] - n))]
                                                  (log {:cmd       :sale
                                                        :food      item2
                                                        :amount    n
                                                        :money     k
                                                        :seller-id other-agent-id
                                                        :agent-id  id})
                                                  (if (= n amount) ;;will buy the full order
                                                    (recur (-> acc2
                                                               (update-in [:foods item2 :order-cur] inc)
                                                               (ut/dissoc-in [:orders [item2 order-cur]]))
                                                           (- goal n))
                                                    (update-in acc2 [:orders [item2 order-cur] :amount] - n))))
                                              acc2))
                                          acc2))))
                                  acc
                                  (keys foods))
                  burgers (apply min
                                 (for [food (keys foods)]
                                   (get-in acc [:agent-foods [id food] :owned])))]
              (log {:cmd      :eat
                    :agent-id id
                    :burgers  burgers})
              (reduce (fn [acc2 item2]
                        (update-in acc2 [:agent-foods [id item2] :owned] - burgers))
                      acc
                      (keys foods))))
          state
          (shuffle (vals agents))))

(defn sleep [{:keys [foods]
              :as   state}
             {:keys [mammoth-price]
              :as   options}]
  (-> (update state
              :agent-foods
              (fn [af]
                (into {}
                      (for [[k v] af]
                        [k  (update v :owned (fn [n] 0 #_(int (* n 0.75))))]))))
      (assoc :orders {})
      (assoc :foods
             (sn/modify {:_ (fn [{:keys [price
                                         order-cur
                                         order-num
                                         id]
                                  :as   food}]
                              {:id        id
                               :order-cur 0
                               :order-num 0
                               :price     (if (and (= id :mammoth) mammoth-price)
                                            (min mammoth-price
                                                 (* price
                                                    (if (= order-cur order-num)
                                                      1.05
                                                      0.95)))
                                            (* price
                                               (if (= order-cur order-num)
                                                 1.05
                                                 0.95)))})}
                        foods))))

(defn turn [state options]
  (-> state
      (work options)
      eat
      (sleep options)))

(defn money-total [{:keys [agents]
                    :as   state}]
  (apply + (map :money (vals agents))))

(defn foods-total [food
                   {:keys [agent-foods]
                    :as   state}]
  (apply + (map (fn [{:keys [id
                             owned]}]
                  (if (= (second id) food)
                    owned
                    0))
                (vals agent-foods))))

(defn main []
  (println "start")
  (let [k (iterate (fn [state]
                     (let [state (turn state {})
                           log (purge-log)]
                       (println "burgers " (apply + (keep :burgers log)))
                       state))
                   (init-state))
        states (take 60 k)]
    (doseq [state states]
      #_(pp/pprint (sn/query {:foods {:_ {:id     nil
                                          :price nil}}}
                             state))
      #_(println "money=" (money-total state))
      #_(println "burgers=" (burger-total state))
      #_(println "mammoth=" (foods-total :mammoth state))
      #_(println "ketchup=" (foods-total :ketchup state))
      #_(println "lettuce=" (foods-total :lettuce state))
      #_(println "bread=" (foods-total :bread state)))
    #_(last states)
    (pp/pprint (sn/query {:foods {:_ {:id     nil
                                      :price nil}}}
                         (last states)))
    nil))

(def turn-no-opt #(turn % {}))
(defn main2 []
  (take 10 (iterate turn-no-opt (init-state))))


(defn get-agents-foods [state agent-id]
  (into {} (filter (fn [[[k]]] (= agent-id k)) (:agent-foods state))))

(defn get-agents-orders [state lookup-agent-id]
  (filter (fn [{:keys [id agent-id]}]
            (or (= lookup-agent-id (second id))
                (= lookup-agent-id agent-id)))
          (vals (:orders state))))

(defn summarize-agent-6 [state]
  {:agent (get-in state [:agents 6])
   :agent-food (get-agents-foods state 6)
   :orders (get-agents-orders state 6)})

(def ten-turns (last (take 10 (iterate turn-no-opt (init-state)))))
(def after-working (work ten-turns {}))

(summarize-agent-6 ten-turns)
(summarize-agent-6 after-working)

(def after-eating (eat after-working))
(summarize-agent-6 after-eating)

(def after-sleeping (sleep after-eating {}))
(summarize-agent-6 after-sleeping)

(:foods after-eating)
; {:mammoth
;  {:id :mammoth,
;   :order-cur 14,
;   :order-num 14,
;   :price 210.53740073994146},
;  :ketchup
;  {:id :ketchup, :order-cur 5, :order-num 5, :price 94.53741145869138},
;  :lettuce
;  {:id :lettuce,
;   :order-cur 8,
;   :order-num 11,
;   :price 127.64411248271482},
;  :bread
;  {:id :bread, :order-cur 8, :order-num 10, :price 104.48871792802733}}
(:foods after-sleeping)
; {:mammoth
;  {:id :mammoth, :order-cur 0, :order-num 0, :price 221.06427077693854},
;  :ketchup
;  {:id :ketchup, :order-cur 0, :order-num 0, :price 99.26428203162595},
;  :lettuce
;  {:id :lettuce, :order-cur 0, :order-num 0, :price 121.26190685857907},
;  :bread
;  {:id :bread, :order-cur 0, :order-num 0, :price 99.26428203162595}}

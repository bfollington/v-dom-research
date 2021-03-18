(ns v-dom-research.core
  (:require [clojure.data :as d]
            [clojure.pprint :as pp]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def actors (atom {}))

(declare inflate)
(declare render-child)

(defn render-children [id children]
  (vec (map-indexed (partial render-child id) children)))

(defn apply-initial-state [node]
  (assoc node :state ((:init-fn node))))

(defn render-scene [id scene]
  (let [[root-type props & children] scene
        ctor (inflate root-type)
        node (ctor (merge props {:id id :children children}))
        existing-node (get @actors id (apply-initial-state node))
        ;; _ (println "?" id (get @actors id))
        ;; _ (pp/pprint (apply-initial-state node))
        render-fn (:render-fn existing-node)]

    (assoc existing-node :children (render-children id (render-fn existing-node)))))

(defn render-child [pid i s]
  (let [[_ props & _] s
        explicit-id (get props :id i)]
    (render-scene (str pid "/" explicit-id) s)))

(defn render-nil [_] [])
(defn update-noop [{:keys [state]}] state)

(defn sprite [{:keys [id src] :as props}]
  {:id id
   :type :entity/sprite
   :init-fn (fn [] (atom {:x 0 :y 0}))
   :update-fn (fn [{:keys [state]}] (swap! state update :x inc))
   :render-fn render-nil
   :src src})

(defn prefab [{:keys [id] :as props}]
  {:id id
   :type :entity/prefab
   :init-fn (fn [] (atom nil))
   :update-fn update-noop
   :render-fn (fn [props] [[:sprite {:src "1.png"}] [:sprite {:src "2.png"}]])})

(defn multi-sprite [{:keys [id count]}]
  {:id id
   :type :entity/multi-sprite
   :init-fn (fn [] (atom {:count 1}))

   :update-fn (fn [{:keys [state]}]
                (swap! state update :count inc)
                (swap! state update :count (fn [c] (if (> c 10) 0 c))))

   :render-fn (fn [{:keys [state]}]
                (vec (for [i (range (:count @state))]
                       [:sprite {:src (str i ".png")}])))})

(defn container [{:keys [id children] :as props}]
  {:id id
   :type :entity/container
   :init-fn (fn [] (atom nil))
   :update-fn update-noop
   :render-fn (fn [props] children)})

(def inflate
  {:sprite sprite
   :container container
   :prefab prefab
   :multi-sprite multi-sprite})

(def scene-a
  [:container {:x 0 :y 0}
   [:sprite {:id "player" :src "player.png"}]
   [:multi-sprite {:count 1}]
   [:multi-sprite {:count 1}]
   [:multi-sprite {:count 1}]
   [:multi-sprite {:count 1}]
   [:multi-sprite {:count 1}]
   [:prefab {:id "prefab"}]])

(def scene-b
  [:container {:x 10 :y 0}
   [:sprite {:id "player" :src "player.png"}]
   [:sprite {:src "enemy.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:sprite {:src "another.png"}]
   [:prefab {:id "prefab-2"}]
   [:container {:x 12 :y 0}
    [:prefab {}]
    [:sprite {:src "another.png"}]
    [:sprite {:src "another.png"}]
    [:sprite {:src "another.png"}]
    [:sprite {:src "another.png"}]
    [:sprite {:src "another.png"}]
    [:sprite {:src "another.png"}]
    [:container {:x 12 :y 0}
     [:prefab {}]
     [:sprite {:src "another.png"}]
     [:sprite {:src "another.png"}]
     [:sprite {:src "another.png"}]
     [:sprite {:src "another.png"}]
     [:sprite {:src "another.png"}]
     [:sprite {:src "another.png"}]]]])


;; Q: diff the inflated version or the collapsed version?

(def EMPTY [:container {}])
(defn initial-scene [] (render-scene "0" EMPTY))
(def scene (atom (initial-scene)))

(defn mount-node! [{:keys [id] :as node}]
  (println "+" id)
  (swap! actors assoc id node))

(defn unmount-node! [{:keys [id] :as node}]
  (println "-" id)
  (swap! actors dissoc id))

(defn walk-tree [entry f]
  (let [children (:children entry)]
    (doseq [c children]
      (when c
        (when (:id c)
          (f c))
        (walk-tree c f)))))

(defn diff-scene [new-scene]
  (let [current-scene @scene
        [removed added unchanged] (d/diff current-scene new-scene)]
    (walk-tree removed unmount-node!)
    (walk-tree added mount-node!)

    (reset! scene new-scene)))



(defn update-actor [node]
  (let [update-fn (:update-fn node)]
    (update-fn node)))

(defn update-actors! []
  (doseq [[_k a] @actors]
    (update-actor a)))

(defn update! []
  (let [_ (time (update-actors!))
        current-scene @scene
        render-fn (:render-fn current-scene)
        children (render-fn current-scene)
        new-scene (assoc current-scene :children (render-children "0" children))
        _ (time (diff-scene new-scene))]))

(defn do-work []
  (reset! actors {})
  (reset! scene (initial-scene))
  (diff-scene (render-scene "0" scene-a))
  (pp/pprint @actors)
  (println "done"))

(do-work)
;; (update!)

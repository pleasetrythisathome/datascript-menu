(ns datascript-menu
  (:require
   [clojure.string :as str]
   [sablono.core :as s :include-macros true]
   [datascript :as d]
   [rum :include-macros true]
   [datascript-menu.gen :as gen]))

(enable-console-print!)

(def ^:private schema {:guest/order    {:db/valueType :db.type/ref}
                       :order/position {:db/valueType :db.type/ref
                                        :db/cardinality :db.cardinality/many}})

(def conn (d/create-conn schema))

;;;; Reactions

(def reactions (atom {}))

(defn listen-for! [key path fragments callback]
  (swap! reactions assoc-in (concat [path] fragments [key]) callback))

(defn unlisten-for! [key path fragments]
  (swap! reactions update-in (concat [path] fragments) dissoc key))

(d/listen! conn
           (fn [tx-data]
             (doseq [datom (:tx-data tx-data)
                     [path m] @reactions
                     :let [fragments ((apply juxt path) datom)]
                     [key callback] (get-in m fragments)]
               (callback datom tx-data key))))

;;;; Fixtures

(d/transact! conn (map (fn [[n d]] {:position/name n
                                    :position/desc d})
                       gen/positions))

(def position-ids (map first (d/q '[:find ?e :where [?e :position/name]] @conn)))

(dotimes [i 100]
  (let [guests (repeatedly (+ (rand-int 4) 1)
                           (fn [] {:guest/name (gen/gen-name)
                                   :guest/order -1}))
        order {:db/id -1
               :order/id (+ 9999 (rand-int 90000))
               :order/position (take (+ (rand-int 3) 1) (shuffle position-ids))}]
    (d/transact! conn (into [order] guests))))

;;;; Views


;; Reusable mixin that subscribes to the part of DB
(defn listen-for-mixin [path-fn]
  {:did-mount
   (fn [state]
     (let [[args path] (apply path-fn (:rum/args state))
           key         (rand)
           comp        (:rum/react-component state)
           callback    (fn [datom tx-data key] (rum/request-render comp))]
       (listen-for! key args path callback)
       (assoc state ::listen-path [key args path])))
   :will-unmount
   (fn [state]
     (apply unlisten-for! (::listen-path state)))})

(def query-mixin
  {:will-mount
   (fn [state]
     (print :mount)
     (assoc state :query/args (:rum/args state)))
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (let [[query db & inputs] (:query/args state)
             entities (->> (apply d/q query db inputs)
                           (mapv (comp (partial d/entity db)
                                       first)))]
         (-> state
             (assoc :rum/args [entities])
             (render-fn)))))
   :will-unmount
   (fn [state]
     ;;(apply unlisten-for! (::listen-path state))
     )})

(def position-view-mixin (listen-for-mixin (fn [pid] [[:e :a] [pid :position/name]]))) ;; concrete mixin

(rum/defc position-view < position-view-mixin [pid]
  (let [p (d/entity @conn pid)]
    [:li.position
     (:position/name p)
     [:span.id (:db/id p)]]))

(rum/defc order [order]
  [:.order
   (str "Order #" (:order/id order))
   [:span.id (:db/id order)]
   [:ul.positions
    (for [p (:order/position order)]
      (position-view (:db/id p)))]])

(rum/defc person [guest]
  [:.person
   (:guest/name guest)
   [:span.id (:db/id guest)]
   (order (:guest/order guest))])

(rum/defc guest-list < query-mixin
  [guests]
  [:.guests
   [:h1 "Guests"]
   (for [guest (sort-by :guest/name guests)]
     (person guest))])

(rum/defc position-edit [position]
  [:.position-edit
   [:input {:type "text"
            :value (:position/name position)
            :on-change (fn [e]
                         (let [new-val (.. e -target -value)]
                           (d/transact! conn [[:db/add (:db/id position) :position/name new-val]])))}]
   [:span.id (:db/id position)]])

(rum/defc page [db]
  [:.page
   (guest-list '[:find ?e
                 :where [?e :guest/name]]
               db)
   [:.menu
    [:h1 "Menu"]
    #_(for [[eid] (sort (d/q '[:find ?e :where [?e :position/name]] db))]
      (position-edit (d/entity db eid)))]])

(rum/mount (page @conn) (.-body js/document))

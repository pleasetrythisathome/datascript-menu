(ns datascript-menu
    (:require-macros
   [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [clojure.string :as str]
   [sablono.core :as s :include-macros true]
   [cljs.core.async :refer [<! timeout]]
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

(defn query-key [q inputs]
  (prn-str [q inputs]))

(defn bind
  [conn q inputs callback]
  (d/listen! conn (query-key q inputs)
             (fn [{:keys [tx-data db-after]}]
               (let [novelty (apply d/q q tx-data inputs)]
                 (when (not-empty novelty) ;; Only update if query results have changed
                   (callback (apply d/q q db-after inputs)))))))

(defn unbind
  [conn q inputs]
  (d/unlisten! conn (query-key q inputs)))

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
  {:will-mount
   (fn [state]
     (let [comp  (:rum/react-component state)
           paths (for [eid (:rum/args state)]
                   (let [[args path] (apply path-fn (:rum/args state))
                         key         (rand)
                         callback    (fn [datom tx-data key]
                                       (rum/request-render comp))]
                     (listen-for! key args path callback)
                     [key args path]))]
       (assoc state
              ::listen-path paths)))
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (-> state
           (update :rum/args (partial map (partial d/entity @conn)))
           (render-fn))))
   :will-unmount
   (fn [state]
     (doseq [path (::listen-paths state)]
       (apply unlisten-for! path)))})

(def ^:dynamic *queries*)

(def query-reactive
  {:transfer-state
  (fn [old new]
    (assoc new ::queries (::queries old)))
  :wrap-render
  (fn [render-fn]
    (fn [state]
      (binding [*queries*      (volatile! #{})]
        (let [comp             (:rum/react-component state)
              old-queries      (::queries state #{})
              [dom next-state] (render-fn state)
              new-queries      @*queries*]
          (doseq [[query ref] old-queries]
            (when-not (contains? new-queries query)
              (unbind conn query [])))
          (doseq [query new-queries]
            (when-not (contains? old-queries query)
              (bind conn query []
                    (fn [_]
                      (rum/request-render comp)))))
          [dom (assoc next-state ::queries new-queries)]))))
  :will-unmount
  (fn [state]
    (doseq [query (::queries state)]
      (unbind conn query []))
    (dissoc state ::queries))})

(defn query [query]
  (vswap! *queries* conj query)
  (let [db @conn]
    (->> (d/q query db)
         (mapv (comp (partial d/entity db)
                     first)))))

(def position-view-mixin (listen-for-mixin (fn [pid] [[:e :a] [pid :position/name]]))) ;; concrete mixin

(rum/defc position-view < position-view-mixin
  [p]
  [:li.position
   (:position/name p)
   [:span.id (:db/id p)]])

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

(rum/defc position-edit [position]
  [:.position-edit
   [:input {:type "text"
            :value (:position/name position)
            :on-change (fn [e]
                         (let [new-val (.. e -target -value)]
                           (d/transact! conn [[:db/add (:db/id position) :position/name new-val]])))}]
   [:span.id (:db/id position)]])

(rum/defc sorted-list
  [view sort-fn items]
  [:div
   (for [item (sort-by sort-fn items)]
     (view item))])

(rum/defc page < query-reactive
  [db]
  [:.page
   [:.guests
    [:h1 "Guests"]
    (sorted-list person :guest/name (query '[:find ?e
                                             :where [?e :guest/name]]))]
   [:.menu
    [:h1 "Menu"]
    (sorted-list position-edit :db/id (query '[:find ?e
                                               :where [?e :position/name]]))]])

(rum/mount (page @conn) (.-body js/document))

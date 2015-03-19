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

(defn entity-exists? [db eid]
  (get (set (mapv first (:eavt db))) eid))

(defn bind
  [conn q inputs callback]
  (d/listen! conn (query-key q inputs)
             (fn [{:keys [tx-data db-before db-after]}]
               (let [novelty (apply d/q q tx-data inputs)]
                 (when (and (not-empty novelty)
                            (->> novelty
                                 (mapcat identity)
                                 (mapv (juxt (partial entity-exists? db-before)
                                             (partial entity-exists? db-after)))
                                 (mapv (partial apply =))
                                 (reduce #(and %1 %2))
                                 (not)))
                   ;; Only update if entity list has changed
                   (callback (apply d/q q db-after inputs)))))))


(defn unbind
  [conn q inputs]
  (d/unlisten! conn (query-key q inputs)))

;;;; Fixtures

(d/transact! conn (map (fn [[n d]] {:position/name n
                                    :position/desc d})
                       gen/positions))

(def position-ids (map first (d/q '[:find ?e :where [?e :position/name]] @conn)))

(defn add-fixtures! [n]
  (dotimes [i n]
    (let [guests (repeatedly (+ (rand-int 4) 1)
                             (fn [] {:guest/name (gen/gen-name)
                                     :guest/order -1}))
          order {:db/id -1
                 :order/id (+ 9999 (rand-int 90000))
                 :order/position (take (+ (rand-int 3) 1) (shuffle position-ids))}]
      (d/transact! conn (into [order] guests)))))

(defn do-every! [freq f n]
  (go-loop [tick (timeout 0)
            runs 1]
    (<! tick)
    (f)
    (when-not (= runs n)
      (recur (timeout freq) (inc runs)))))

(do-every! 1000 (partial add-fixtures! 1) 10)
;;(add-fixtures! 100 5)

;;;; Views

;; Reusable mixin that subscribes to the part of DB
(defn listen-for-mixin [path-fn]
  {:will-mount
   (fn [state]
     (let [comp  (:rum/react-component state)
           paths (doall
                  (for [eid (:rum/args state)]
                    (let [[args path] (apply path-fn (:rum/args state))
                          key         (rand)
                          callback    (fn [datom tx-data key]
                                        (rum/request-render comp))]
                      (listen-for! key args path callback)
                      [key args path])))]
       (assoc state
              ::listen-path paths)))
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (let [eids (:rum/args state)
             entities (mapv (partial d/entity @conn) eids)
             [dom next-state] (render-fn (assoc state :rum/args entities))]
         [dom (assoc next-state :rum/args eids)])))
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
  (mapcat identity (d/q query @conn)))

(rum/defc position-view < (listen-for-mixin (fn [pid]
                                              [[:e :a] [pid :position/name]]))
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
      (rum/with-props position-view (:db/id p)
        :rum/key (:db/id p)))]])

(rum/defc person < (listen-for-mixin (fn [pid]
                                        [[:e :a] [pid :guest/order]]))
  [guest]
  [:.person
   (:guest/name guest)
   [:span.id (:db/id guest)]
   (rum/with-props order (:guest/order guest)
     :rum/key (get-in guest [:guest/order :db/id]))])

(rum/defc position-edit < (listen-for-mixin (fn [pid]
                                              [[:e :a] [pid :db/id]]))
  [position]
  [:.position-edit
   [:input {:type "text"
            :value (:position/name position)
            :on-change (fn [e]
                         (let [new-val (.. e -target -value)]
                           (d/transact! conn [[:db/add (:db/id position) :position/name new-val]])))}]
   [:span.id (:db/id position)]])

(rum/defc sorted-list
  [view sort-fn eids]
  [:div
   (for [eid (sort-by sort-fn eids)]
     (rum/with-props view eid
       :rum/key eid))])

(rum/defc page < query-reactive
  [conn]
  [:.page
   [:.guests
    [:h1 "Guests"]
    (sorted-list person (comp :guest/name
                              (partial d/entity @conn))
                 (query '[:find ?e
                          :where [?e :guest/name]]))]
   [:.menu
    [:h1 "Menu"]
    (sorted-list position-edit identity
                 (query '[:find ?e
                          :where [?e :position/name]]))]])

(rum/mount (page conn) (.-body js/document))

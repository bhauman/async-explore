(ns todos-first.client
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! put!]]
   [jayq.core :refer [ajax]]
   [jayq.util :refer [log]]
   [clojure.string :refer [join]])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))

(def host "http://scratch.leaves.io")

(defn- request-options [data]
  {:type "POST"
   :contentType "text/plain"
   :data (.stringify js/JSON (clj->js data))})

(defn- interface-path [interface id path]
  (str host "/json-doc/" id "/" (name interface) "/data-path/" (join "/" (map name path))))

(defn- service-interface-request [interface id path data]
  (ajax (interface-path interface id path) (request-options data)))

(defn service-new [data]
  (ajax (str host "/json-doc") 
        (request-options data)))

(defn service-assoc [id path data]
  (service-interface-request :settable id path data))

(defn service-add [id path data]
  (service-interface-request :addable id path data))

(defn service-move [id path data]
  (service-interface-request :movable id path data))

(defn service-remove [id path]
  (ajax (interface-path :deletable id path)
        {:type "DELETE"
         :contentType "text/plain"}))

(defn service-get [id]
  (ajax (str host "/json-doc/" id "?with_data=true") {:type "GET"}))

(declare doc-assoc doc-remove)

(defprotocol RemoteImmutable
  (remote-id [x])
  (remote-parent-id [x]))

(deftype JsonDoc [id parent-id data]
  RemoteImmutable
  (remote-id [x] id)
  (remote-parent-id [x] parent-id)  
  ILookup
  (-lookup [this key]
    (-lookup data key))
  (-lookup [this key not-found]
    (-lookup data key not-found))
  IAssociative
  (-contains-key? [this k]
    (-contains-key? data k))
  (-assoc [this k v]
    (doc-assoc this [k] v))
  IMap
  (-dissoc [this k]
    (doc-remove this [k]))
  IEquiv
  (-equiv [o other]
    (-equiv (seq data) (seq other)))
  ISeqable
  (-seq [o] (-seq data))
  IPrintWithWriter
  (-pr-writer [o writer _] (-write writer data))  
)

(defn request-chan [request handler]
  (let [rc (chan)]
    (-> request
        (.done #(put! rc (handler (js->clj % :keywordize-keys true))))
        (.fail #(close! rc)))
    rc))

(defn result->json-doc [data]
  (log (prn-str data))
  (JsonDoc. (data :_id) (data :parent-id) (data :data)))

(defn channel-it [request]
  (request-chan request result->json-doc))

(defn doc-create [data]
  (channel-it (service-new data)))

(defn doc-assoc [json-doc path data]
  (channel-it (service-assoc (remote-id json-doc) path data)))

(defn doc-add [json-doc path data]
  (channel-it (service-add (remote-id json-doc) path data)))

(defn doc-move [json-doc path data]
  (channel-it (service-move (remote-id json-doc) path data)))

(defn doc-remove [json-doc path]
  (channel-it (service-remove (remote-id json-doc) path)))

(defn doc-get [id]
  (channel-it (service-get id)))


;; testing is below here

(defn dassert [phrase x]
  (if x
    (.log js/console "passed: "  phrase )
    (.log js/console "FAILED: "  phrase )))

(defn tests []
  (go
   (.log js/console "TESTing service")


   (let [doc (<! (doc-create {:hey "there"}))]
     (dassert "doc-create" (= doc {:hey "there"}))
     (dassert "doc-remote-id" (not= nil (remote-id doc)))
     (dassert "doc-remote-parent-id" (= nil (remote-parent-id doc)))          
     (let [next-doc (<! (doc-assoc doc [:hi] "gabby"))]
       (dassert "doc-assoc" (= next-doc {:hey "there" :hi "gabby"}))
       (dassert "doc-assoc-remote-id" (not= nil (remote-id next-doc)))
       (dassert "doc-assoc-remote-parent-id" (=  (remote-id doc)
                                                 (remote-parent-id next-doc)))          
       (let [add-doc (<! (doc-add next-doc [:hiy] "gabby2"))]
         (dassert "doc-add" (= add-doc {:hey "there" :hi "gabby" :hiy "gabby2"}))
         (dassert "doc-add-remote-id" (not= nil (remote-id add-doc)))
         (dassert "doc-add-remote-parent-id" (=  (remote-id next-doc)
                                                 (remote-parent-id add-doc)))
         (let [move-doc (<! (doc-move add-doc [:hiy] "hiiy"))]
           (dassert "doc-move" (= move-doc {:hey "there" :hi "gabby" :hiiy "gabby2"}))
           (dassert "doc-move-remote-id" (not= nil (remote-id move-doc)))
           (dassert "doc-move-remote-parent-id" (=  (remote-id add-doc)
                                                    (remote-parent-id move-doc)))
           (let [remove-doc (<! (doc-remove move-doc [:hiiy]))]
             (dassert "doc-remove" (= remove-doc {:hey "there" :hi "gabby"}))
             (dassert "doc-remove-remote-id" (not= nil (remote-id remove-doc)))
             (dassert "doc-remove-remote-parent-id" (=  (remote-id move-doc)
                                                      (remote-parent-id remove-doc)))          
             )
           (let [get-doc (<! (doc-get (remote-id move-doc)))]
             (dassert "doc-get" (= get-doc move-doc))
             (dassert "doc-get-remote-id" (= (remote-id get-doc) (remote-id move-doc)))
             (dassert "doc-get-remote-parent-id" (=  (remote-id add-doc)
                                                      (remote-parent-id get-doc)))
             )
           )
         ))
     
     )
   
   (let [doc (<! (doc-create {:hey 5 :b {:c 3}}))]
     (dassert "get" (= 5 (get doc :hey)))
     (dassert "get-in" (= 3 (get-in doc [:b :c])))
     (dassert "assoc" (= 2 (get (<! (assoc doc :harry 2)) :harry)))
     (dassert "assoc keyword lookup" (= 2 (get (<! (assoc doc :harry 2)) :harry)))
     (dassert "dissoc" (= nil (get (<! (dissoc doc :hey)) :hey)))
     (dassert "eqiv"  (= (JsonDoc. 3 5 {:are 3}) (JsonDoc. 3 nil {:are 3})))
     )

   ))

#_(tests)

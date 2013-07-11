(ns todos-first.core
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put!]]
   [jayq.core :refer [$ append ajax inner $deferred when done resolve pipe on] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate]
   [clojure.string :refer [join blank?]]
   [todos-first.templates :refer [app-view]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))


(def dlog (comp log prn-str))

(defn click-chan [input-chan selector ev-name]
  (on ($ "body") :click selector {} (fn [e] (jq/prevent e) (put! input-chan [ev-name]))))

(defn fields-value-map [form-selector fields]
  (into {} (map
            (fn [fld]
              [fld (jq/val ($ (str form-selector " input[name=" (name fld) "]")))] )
            fields)))

(defn form-submit-chan [input-chan form-selector ev-name fields]
  (on ($ "body") :submit form-selector {}
      (fn [e]
        (jq/prevent e)
        (put! input-chan [ev-name (fields-value-map form-selector fields)]))))

(defn filter-chan [pred channel]
  (go (loop []
        (let [res (<! channel)]
          (if (pred res) res (recur))))))

(defn filter-events [name channel]
  (let [name-set (if (set? name) name #{name})]
    (filter-chan #(and (pos? (count %)) (name-set (first %)))
                 channel)))

(defn render-page [state]
  (-> ($ ".container")
      (inner (crate/html (app-view state)))))

(defn validate-edit-form [fields]
  (if (blank? (:name fields))
    [[:name "can't be blank"]]
    []))

(defn validate-task-form [fields]
  (if (blank? (:content fields))
    [[:content "can't be blank"]]
    []))

(defn do-add-list-form [input-chan state]
  (go
   (loop [form-state (assoc state
                       :mode :add-list
                       :new-list-form {})]
     (render-page form-state)
     (let [[event-name form-result] (<! (filter-events
                                         #{:new-list-form-submit :cancel-new-list-form}
                                         input-chan))]
       (if (= event-name :cancel-new-list-form)
         state
         (let [errors (validate-edit-form form-result)]
           (if (pos? (count errors))
             (recur (-> form-state (assoc-in [:new-list-form :name]
                                             (form-result :name))
                        (assoc-in [:new-list-form :errors] errors)))
             (let [data (state :data)
                   new-sym (-> "todo-list" gensym name keyword)
                   new-data (assoc-in data [:todo-lists new-sym]
                                      {:name (form-result :name)
                                       :tasks {}})]
               (assoc state
                 :data new-data
                 :focused-list new-sym))                                      
             )))))))

(defn do-add-task [state form-vals]
  (let [errors (validate-task-form form-vals)]
    (if (pos? (count errors))
      (assoc state :new-task-form { :content (:content form-vals)
                                    :errors  errors })
      (let [focused-list-id (state :focused-list)
            new-task-id (-> "task" gensym name keyword)
            new-data (assoc-in (state :data) [:todo-lists
                                              focused-list-id
                                              :tasks
                                              new-task-id]
                               { :content (form-vals :content)})]
        (assoc state :data new-data :new-task-form {}))
      )
    ))

(defn app-loop [start-state]
  (let [input-chan (chan)]
    (click-chan       input-chan ".nav .new-list" :get-new-list)
    (form-submit-chan input-chan ".new-list-form" :new-list-form-submit [:name])
    (click-chan       input-chan ".cancel-new-list" :cancel-new-list-form)
    (form-submit-chan input-chan ".new-task-form" :new-task-form-submit [:content])
    (go
     (loop [state start-state]
       (log "loop top" (prn-str state))
       (render-page state)
       (let [event (<! input-chan)]
         (condp = (first event)
           :get-new-list (recur (<! (do-add-list-form input-chan state)))
           :new-task-form-submit (recur (do-add-task state (last event)))
           (recur state)
          ))))))

(defn setup-page []
  (app-loop {}))

(setup-page)


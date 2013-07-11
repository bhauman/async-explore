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
  (on ($ "body") :click selector {}
      (fn [e] (jq/prevent e)
        (let [data (-> e .-currentTarget $ .data
                       (js->clj :keywordize-keys true))]
          (put! input-chan [ev-name data])))))

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

(defn filter-events [name-set channel]
  (let [name-set (if (set? name-set) name-set #{name-set})]
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

(defn add-list [{:keys [data] :as state} list-name]
  (let [new-sym (-> "todo-list-" gensym name keyword)
        new-data (assoc-in data [:todo-lists new-sym]
                           {:name list-name :tasks {}})]
    (assoc state
      :data new-data
      :focused-list new-sym)))

(defn add-list-form-app [input-chan state]
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
             (recur (assoc form-state :new-list-form {:name (form-result :name)
                                                      :errors errors}))
             (add-list state (form-result :name)))))))))

(defn add-task [{:keys [focused-list] :as state} content]
  (let [new-task-id (-> "task" gensym name keyword)
        new-data (assoc-in (state :data) [:todo-lists
                                          focused-list
                                          :tasks
                                          new-task-id
                                          :content]
                           content)]
    (assoc state :data new-data)))

(defn do-add-task [state form-vals]
  (let [errors (validate-task-form form-vals)]
    (if (pos? (count errors))
      (assoc state :new-task-form { :content (:content form-vals)
                                    :errors  errors })
      (-> state
          (add-task (:content form-vals))
          (dissoc :new-task-form)))))

(defn select-list [state {:keys [listId]}]
  (assoc state :focused-list (keyword listId)))

(defn delete-task [{:keys [data] :as state} {:keys [taskId]}]
  (assoc state :data
         (update-in data
                    [:todo-lists
                     (:focused-list state)
                     :tasks] dissoc (keyword taskId))))

(defn complete-task [{:keys [data] :as state} {:keys [taskId]}]
  (assoc state :data
         (assoc-in data
                   [:todo-lists
                    (:focused-list state)
                    :tasks
                    (keyword taskId)
                    :completed] true )))

(defn delete-list [{:keys [data] :as state} {:keys [listId]}]
  (let [updated-data (update-in data
                                [:todo-lists] dissoc (:focused-list state))]
    (assoc state
      :data updated-data
      :focused-list (-> updated-data :todo-lists first first))))

(defn app-loop [start-state]
  (let [input-chan (chan)]
    (click-chan       input-chan ".nav .new-list" :get-new-list)
    (form-submit-chan input-chan ".new-list-form" :new-list-form-submit [:name])
    (click-chan       input-chan ".cancel-new-list" :cancel-new-list-form)
    (form-submit-chan input-chan ".new-task-form" :new-task-form-submit [:content])
    (click-chan       input-chan ".list-nav a"   :select-list)
    (click-chan       input-chan "a.delete-list" :delete-list)    
    (click-chan       input-chan "a.delete-task" :delete-task)
    (click-chan       input-chan "a.complete-task" :complete-task)    
    (go
     (loop [state start-state]
       (log "loop top " (prn-str state))
       (render-page state)
       (let [[ev-name ev-data] (<! input-chan)]
         (condp = ev-name
           :get-new-list (recur (<! (add-list-form-app input-chan state)))
           :new-task-form-submit (recur (do-add-task state ev-data))
           :select-list (recur (select-list state ev-data))
           :delete-list (recur (delete-list state ev-data))           
           :delete-task (recur (delete-task state ev-data))
           :complete-task (recur (complete-task state ev-data))           
           (recur state)
          ))))))

(defn setup-page []
  (app-loop {}))

(setup-page)

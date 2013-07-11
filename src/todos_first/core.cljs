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

(defn click-chan [selector value]
  (let [rc (chan (sliding-buffer 1))]
    (on ($ "body") :click selector {} (fn [e] (jq/prevent e) (put! rc value)))
    rc))

(defn fields-value-map [form-selector fields]
  (into {} (map
            (fn [fld]
              [fld (jq/val ($ (str form-selector " input[name=" (name fld) "]")))] )
            fields)))

(defn form-submit-chan [form-selector fields]
  (let [rc (chan)]
    (on ($ "body") :submit form-selector {}
        (fn [e]
          (jq/prevent e)
          (put! rc (fields-value-map form-selector fields))))
    rc))

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

(defn app-loop [start-state]
  (let [add-list-click (click-chan ".nav .new-list" :add-list)
        new-list-form-submit (form-submit-chan ".new-list-form" [:name])
        cancel-new-list-form (click-chan ".cancel-new-list" :add-list)
        new-task-form-submit (form-submit-chan ".new-task-form" [:content])
        do-add-list-form (fn [state]
                           (go
                            (loop [form-state (assoc state
                                                :mode :add-list
                                                :new-list-form {})]
                              (render-page form-state)
                              (let [[form-result ch] (alts! [new-list-form-submit cancel-new-list-form])]
                                (if (= ch cancel-new-list-form)
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
        do-add-task (fn [state form-vals]
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
                            (assoc state :data new-data))
                          )
                        ))
        ]
    
    (go
     (loop [state start-state]
       (log "loop top" (prn-str state))
       (render-page state)
       (let [[e ch] (alts! [add-list-click new-task-form-submit])]
         (condp = ch
           add-list-click (recur (<! (do-add-list-form state)))
           new-task-form-submit (recur (do-add-task state e))
          )
         )
       
       ))))

(defn setup-page []
  (app-loop {}))

(setup-page)


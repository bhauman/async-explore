(ns todos-first.templates
  (:require
   [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put!]]
   [jayq.core :refer [$ append ajax inner $deferred when done resolve pipe on] :as jq]
   [jayq.util :refer [log]]
   [crate.core :as crate]
   [clojure.string :refer [join blank?]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn nav-bar []
  [:div.navbar
   [:div.navbar-inner
    [:a.brand { :href "#" } "Todos"]
    [:ul.nav
     [:li [:a.new-list {:href "#"} [:i.icon-tasks] " add list"]]]]])

(defn form-errors-view [[[_ er] & xs]]
  [:div.alert {} er])

(defn new-list-form [form-state]
  [:div
   [:h4 "New todo list"]
   [:form.new-list-form
    (if (< 0 (count (:errors form-state)))
      (form-errors-view (:errors form-state)))
    [:input.edit-message  {:type "text" :value (:name form-state) :name "name"}]
    [:p
     [:input {:type "submit" :value "Save" :class "btn btn-primary"}]
     [:a {:href "#" :class " cancel-new-list btn"} "cancel"]]]])

(defn list-item-view [[list-id list] focused-list]
  [:li (if (= focused-list list-id) {:class "active"} {})
   [:a {:href "#" :data-list-id (name list-id)} (:name list)]])

(defn list-nav-view [state]
  (let [focused-list (:focused-list state)
        lists (get-in state [:data :todo-lists])]
    [:ul {:class "nav nav-pills list-nav"} (map list-item-view lists (repeat focused-list))]
    ))

(defn new-task-form [form-state]
  [:form.new-task-form
   (if (< 0 (count (:errors form-state)))
     (form-errors-view (:errors form-state)))
   [:input.new-task-name  {:type "text"
                           :value (:content form-state)
                           :name "content"
                           :placeholder "New Task"}]])

(defn task-item-view [[task-id task]]
  [:li (if (:completed task) {:class "completed"} {})
   [:a.delete-task   {:href "#" :data-task-id (name task-id)} [:i.icon-remove]]
   (if (:completed task)
     [:span.spacer " "]
     [:a.complete-task {:href "#" :data-task-id (name task-id)} [:i.icon-ok]]) 
   [:span.content (str " " (task :content))]])

(defn focused-list-view [state]
  (let [focused-list-id (:focused-list state)
        focused-list (get-in state [:data :todo-lists focused-list-id])]
    [:div.focused-list
     [:h3 (:name focused-list)
      [:a.delete-list {:href "#" :data-list-id (name focused-list-id)}
       [:i {:class "icon-trash"}]]]
     [:ul {:class "unstyled"}
      (map task-item-view (:tasks focused-list))]
     (if (> (-> state :data :todo-lists count) 0)
       (new-task-form (:new-task-form state))) 
     ]))

(defn list-and-nav-view [state]
  [:div
   (list-nav-view state)
   (focused-list-view state)
   ] 
  )

(defn app-view [state]
  [:div
   (nav-bar)
   (if (= (state :mode) :add-list)
     (new-list-form (state :new-list-form))
     (if (pos? (count (-> state :data :todo-lists))) (list-and-nav-view state))
     )])


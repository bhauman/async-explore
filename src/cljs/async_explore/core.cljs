(ns async-explore.core
  (:require [cljs.core.async :as async
             :refer [<! >! chan close! sliding-buffer put!]]
            [crate.core :as crate]
            [jayq.core :as jq :refer [$ css inner html on]]
            [clojure.string :as string])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(defn log [arg]
  (.log js/console arg))

(defn click-chan [selector value]
  (let [rc (chan)]
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

(defn validate-edit-form [fields]
  (if (string/blank? (:message fields))
    [[:message "can't be blank"]]
    []))

(defn app-view [state]
  [:div.app {}
   [:h1.message (:message state)]
   [:a.edit {:href "#"} "edit"]])

(defn form-errors-view [[[_ er] & xs]]
  [:div.alert {} er])

(defn edit-view [state]
  [:div.app {}
   [:h4.message "Edit"]
   [:form.edit-form
    (if (< 0 (count (:form-errors state)))
      (form-errors-view (:form-errors state)))
    [:input.edit-message  {:type "text" :value (:message state) :name "message"}]
    [:p
     [:input {:type "submit" :value "Save" :class "btn btn-primary"}]
     [:a {:href "#" :class " cancel-edit btn"} "cancel"]]]])

(defn render-app [state view-func]
  (let [elem ($ ".container")]
    (log elem)
    (inner elem
          (crate/html (view-func state)))))

(defn app-loop [start-state]
  (let [edit-click       (click-chan ".app a.edit" :edit)
        edit-form-submit (form-submit-chan ".app form.edit-form" [:message])
        cancel-edit-form (click-chan ".app form a.cancel-edit" :cancel)]
    (go ;; main loop
     (loop [state start-state]
       (render-app state app-view)
       (<! edit-click)
       (recur
        (loop [form-state state]
          (render-app form-state edit-view)
          (let [[form-result ch] (alts! [edit-form-submit cancel-edit-form])]
            (if (= ch cancel-edit-form)
              state
              (let [errors (validate-edit-form form-result)]
                (if (< 0 (count errors))
                  (recur (-> form-state (merge form-result) (assoc :form-errors errors)))
                  (merge state form-result)
                  )
                )
              ))
          )
        )))))

(defn setup []
  (.log js/console "Hello"))

(app-loop {:message "hey there" :count 0})

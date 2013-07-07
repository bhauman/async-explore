(defproject async-explore "0.1.0-SNAPSHOT"
  :description "Exploring dev models for async.core"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [core.async "0.1.0-SNAPSHOT"]
                 [crate "0.2.4"]
                 [jayq "2.4.0"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild {
              :builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :simple
                                   :pretty-print true}}]})

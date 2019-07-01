(defproject libra "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [reagent "0.8.1"]
                 [funcool/beicon "5.0.0"]]
  :main ^:skip-aot libra.core
  :source-paths ["src"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

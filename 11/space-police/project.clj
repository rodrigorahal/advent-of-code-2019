(defproject space-police "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [org.clojure/core.async "0.6.532"]]
  :main ^:skip-aot space-police.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

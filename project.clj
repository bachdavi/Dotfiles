(defproject card "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [buddy "1.3.0"]
                 [org.clojure/tools.nrepl "0.2.12"]]
  :main ^:skip-aot card.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :user {:plugins [[cider/cider-nrepl "0.14.0"]]})


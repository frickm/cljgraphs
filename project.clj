(defproject graphs "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.2.0"]
                 [lein-light-nrepl "0.0.9"]
                 [org.clojure/tools.nrepl "0.2.3"]]
  :main ^:skip-aot graphs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
  )

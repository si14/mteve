(defproject mteve "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring "1.1.8"]
                 [com.keminglabs/c2 "0.2.2"]
                 [com.keminglabs/vomnibus "0.3.2"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {
    :builds {
      :main {
        :source-paths ["src/cljs"]
        :compiler {:output-to "resources/public/js/cljs.js"
                   :optimizations :simple
                   :pretty-print true}
        :jar true}}}
  :main mteve.server)

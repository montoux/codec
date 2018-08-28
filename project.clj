(defproject montoux/codec "1.0.1-SNAPSHOT"
  :description "Tools for converting between different unicode encodings in ClojureScript."
  :url "http://github.com/montoux/codec"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.1.7"]]

  :cljsbuild {:builds {}}

  :hooks [leiningen.cljsbuild]

  :profiles
  {:dev {:cljsbuild
         {:test-commands
          {"node" ["node" "target/test/js/test.js"]}
          :builds
          {:test {:source-paths ["src" "test"]
                  :compiler     {:output-to     "target/test/js/test.js"
                                 :output-dir    "target/test/js"
                                 :optimizations :none
                                 :main          "test.node"
                                 :target        :nodejs}}}}}}
  )

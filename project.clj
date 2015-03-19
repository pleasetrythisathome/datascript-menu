(defproject datascript-menu "0.1.0"
  :global-vars  {*warn-on-reflection* true}

  :dependencies [
    [org.clojure/clojure "1.7.0-alpha5"]
    [org.clojure/clojurescript "0.0-3126"]
    [org.clojure/core.async "0.1.346.0-17112a-alpha"]
    [datascript "0.8.1"]
    [rum "0.2.6"]
  ]

  :plugins [
    [lein-cljsbuild "1.0.3"]
  ]

  :cljsbuild {
    :builds [
      { :id "prod"
        :source-paths  ["src"]
        :compiler {
          :preamble      ["react/react.min.js"]
          :output-to     "web/menu.min.js"
          :optimizations :advanced
          :pretty-print  false
        }}
      { :id "dev"
        :source-paths  ["src"]
        :compiler {
          :output-to     "web/menu.js"
          :output-dir    "web/target-cljs"
          :optimizations :none
          :source-map    true
        }}
  ]}
)

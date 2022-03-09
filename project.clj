(defproject org.clj-commons/gloss "0.2.7"
  :description "Speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[manifold/manifold "0.2.3"]
                 [org.clj-commons/byte-streams "0.2.10"]
                 [potemkin/potemkin "0.4.5"]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.3"]]}}
  :cljfmt {:indents {#".*" [[:inner 0]]}}

  :global-vars {*warn-on-reflection* true}
  :javac-options ["-target" "1.8" "-source" "1.8"])

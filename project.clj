(defproject gloss "0.2.6"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[manifold "0.1.3"]
                 [byte-streams "0.2.1"]
                 [potemkin "0.4.3"]]
  :aliases {"all" ["with-profile" "dev:1.9:1.7:1.6:1.5"]}
  :plugins [[lein-codox "0.9.4"]
            [ztellman/lein-cljfmt "0.1.10"]]
  :cljfmt {:indents {#".*" [[:inner 0]]}}
  :codox {:source-uri "https://github.com/ztellman/gloss/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :namespaces [gloss.core gloss.io]}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :profiles {:1.9 {:dependencies [[org.clojure/clojure "1.9.0-master-SNAPSHOT"]]}
             :dev {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ["-server"])

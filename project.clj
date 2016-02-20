(defproject gloss "0.2.5"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[manifold "0.1.2"]
                 [byte-streams "0.2.0"]
                 [potemkin "0.4.3"]]
  :aliases {"all" ["with-profile" "dev:1.9:1.7:1.6:1.5"]}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :profiles {:1.9 {:dependencies [[org.clojure/clojure "1.9.0-master-SNAPSHOT"]]}
             :dev {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :jvm-opts ["-server"])

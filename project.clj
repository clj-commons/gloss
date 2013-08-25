(defproject gloss "0.2.2"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[lamina "0.5.0"]
                 [potemkin "0.3.2"]]
  :aliases {"all" ["with-profile" "1.6,dev:1.3,dev:dev:1.4,dev"]}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :profiles {:1.6 {:dependencies [[org.clojure/clojure "1.6.0-master-SNAPSHOT"]]}
             :dev {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}}
  :jvm-opts ["-server"])

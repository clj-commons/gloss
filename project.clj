(defproject gloss "0.2.2-SNAPSHOT"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [lamina "0.5.0-beta1"]
                 [potemkin "0.1.5"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :multi-deps {:all [[lamina "0.5.0-beta1"]
                     [potemkin "0.1.5"]]
			   "master" [[org.clojure/clojure "1.5.0-master-SNAPSHOT"]]
               "1.2" [[org.clojure/clojure "1.2.1"]]
               "1.3" [[org.clojure/clojure "1.3.0"]]}
  :jvm-opts ["-server"])

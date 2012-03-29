(defproject gloss "0.2.2-SNAPSHOT"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [lamina "0.5.0-SNAPSHOT"]
                 [potemkin "0.1.2"]]
  :jvm-opts ["-server"])

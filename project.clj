(defproject gloss "0.2.0-rc2"
  :description "speaks in bytes, so that you don't have to"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.2.0"]
				 [lamina "0.4.0-rc2"]
				 [potemkin "0.1.0"]]
  :jvm-opts ["-server"])
(defproject org.clj-commons/gloss (or (System/getenv "PROJECT_VERSION") "0.3.2")
  :description "Speaks in bytes, so that you don't have to"

  :url "https://github.com/clj-commons/gloss"
  :scm {:name "git" :url "https://github.com/clj-commons/gloss"}
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}

  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases true}]]

  :dependencies [[manifold/manifold "0.3.0"]
                 [org.clj-commons/byte-streams "0.3.2"]
                 [potemkin/potemkin "0.4.6"]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [org.clojure/test.check "1.1.1"]
                                  [metosin/malli "0.8.9"]]}
             :ci {:javac-options ["-target" "1.8" "-source" "1.8"]
                  :dependencies [[org.clojure/clojure "1.11.1"]
                                 [metosin/malli "0.8.9"]]}}
  :cljfmt {:indents {#".*" [[:inner 0]]}}

  :plugins [[jonase/eastwood "1.2.3"]]
  :eastwood {:exclude-linters [:non-dynamic-earmuffs :unlimited-use :unused-meta-on-macro]}

  :global-vars {*warn-on-reflection* true}
  :javac-options ["-target" "1.8" "-source" "1.8"])

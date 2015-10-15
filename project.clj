(defproject nio "1.0.5-SNAPSHOT"
  :description "Clojure support for java.nio."
  :url "http://github.com/pjstadig/nio/"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[criterium "0.4.3"]]}}
  :global-vars {*warn-on-reflection* true}
  :deploy-repositories [["releases" :clojars]])

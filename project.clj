(defproject com.github.ragnard/hamelito "0.1.0-SNAPSHOT"
  :description "Library for a subset of Haml with hiccup and enlive support"
  :url "http://github.com/ragnard/hamelito"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [hiccup "1.0.2"]
                 [org.blancas/kern "0.5.0"]
                 [cheshire "5.0.1"]]
  :profiles {:1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.0-RC15"]]}}
  :aliases {"all" ["with-profile" "1.4:1.5"]}
  :warn-on-reflection true)

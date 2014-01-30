(defproject sem-logic "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.3"]
                 [clojure-opennlp "0.3.2"]
                 [edu.berkeley.nlp/parser "1.7"]
                 [org.clojure/core.logic "0.8.5"]
                 [org.clojure/core.match "0.2.1"]]
  :jvm-opts ["-Xmx2048m"])

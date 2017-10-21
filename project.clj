(defproject cart "0.1.0-SNAPSHOT"
  :description "Classification and Regression Trees"
  :url "https://zintin.io/trees"
  :license {:name "Apache 2"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-csv "2.0.2"]
                 [com.taoensso/timbre "4.10.0"]]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.10.0-alpha2"]
                                  [walmartlabs/datascope "0.1.1"]
                                  [pjstadig/humane-test-output "0.8.3"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :plugins [[lein-cloverage "1.0.9"]]}})

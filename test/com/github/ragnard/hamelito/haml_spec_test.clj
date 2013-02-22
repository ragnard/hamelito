(ns com.github.ragnard.hamelito.haml-spec-test
  (:require [cheshire.core   :as json]
            [clojure.string  :as string]
            [clojure.java.io :as io]
            [com.github.ragnard.hamelito.parser :as parser])
  (:use [clojure.test :only [deftest is testing]]))

(def haml-spec (json/parse-stream
                (io/reader "resources/haml-spec/tests.json")))

(def included-categories
  #{"basic Haml tags and CSS"
    ;; "boolean attributes"
    "conditional comments"
    ;; "headers"
    ;; "HTML escaping"
    ;; "internal filters"
    "markup comments"
    ;; "Ruby-style interpolation"
    ;; "silent comments"
    "tags with HTML-style attributes"
    "tags with Ruby-style attributes"
    "tags with inline content"
    "tags with nested content"
    "tags with unusual CSS identifiers"
    "tags with unusual HTML characters"
    ;; "whitespace preservation"
    ;; "whitespace removal"
    })

(defn- category->test-name
  [category]
  (-> category
      (string/replace #"\s" "-")
      string/lower-case
      (str "-test")
      symbol))

(defn- parse-test-case
  [[description test-data]]
  (let [haml (test-data "haml")
        html (test-data "html")]
    `(testing ~description
       (parser/parse-tree ~haml))))

(defn- requires-locals?
  [test-spec]
  (get test-spec "locals"))

(defn- build-test-cases
  [test-specs]
  (->> test-specs
       (remove requires-locals?)
       (map parse-test-case)))

(defmacro generate-parse-tests
  []
  `(do
     ~@(for [[category test-specs] haml-spec
             :when (contains? included-categories category)]
         (let [name  (category->test-name category)
               tests (build-test-cases test-specs)]
           `(deftest ~name
              ~@tests)))))


(generate-parse-tests)


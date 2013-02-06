(ns hamelito.enlive
  (:require [hamelito.hiccup        :as h]
            [clojure.java.io        :as io]
            [clojure.string         :as string]))

(defn- resource-stream
  [path]
  (.. clojure.lang.RT baseLoader (getResourceAsStream path)))

(defn haml-resource
  [path]
  (with-open [stream (resource-stream path)]
    (let [html (h/to-html (io/reader stream))]
      (java.io.StringReader. html))))

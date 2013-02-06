(ns hamelito.enlive
  "loading this namespace will enable hamelito for rendering .haml templates
in enlive"
  (:require [hamelito.hiccup        :as h]
            [clojure.java.io        :as io]
            [clojure.string         :as string]
            [net.cgrand.enlive-html :as enlive]))

;;;; ugly hack until enlive has better extensibility

(defn- resource-stream
  [path]
  (-> (clojure.lang.RT/baseLoader) (.getResourceAsStream path)))

(defn- load-haml-resource
  [stream html-loader]
  (let [reader (io/reader stream)
        haml   (string/join "\n" (line-seq reader))]
    (html-loader (java.io.StringReader. (h/to-html haml)))))

(defmethod enlive/get-resource String
  [path loader]
  (let [stream (resource-stream path)]
    (if (.endsWith path ".haml")
      (load-haml-resource stream loader)
      (loader stream))))


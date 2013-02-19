(ns hamelito.enlive
  (:require [hamelito.doctypes :as doctypes]
            [hamelito.parser   :as parser]
            [clojure.string    :as string]
            [clojure.java.io   :as io]))

;; bring in the cond->
(when (< (:minor *clojure-version*) 5)
  (use 'hamelito.util))

(def vec-conj (fnil conj []))

(defn- flat-conj
  [vec xs]
  (apply vec-conj vec xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enlive 

(defprotocol ToEnliveNode
  (-enlive-node [this]))

(defn- element->enlive-node
  [{:keys [name id classes attributes inline-content children] :as element}]
  (cond-> {:tag (or (keyword name) :div)}

          id
          (assoc-in [:attrs :id] id)

          (not (empty? classes))
          (assoc-in [:attrs :class] (string/join " " classes))

          attributes
          (update-in [:attrs] merge attributes)

          (not (string/blank? inline-content))
          (update-in [:content] vec-conj inline-content)

          children
          (update-in [:content] flat-conj (map -enlive-node children))))

(defn- comment->enlive-node
  [{:keys [text condition children]}]
  (when children
    (throw (ex-info "Children to comments not yet supported" {})))
  (cond-> {:tag :comment}

          condition
          (assoc :data (str "[" condition "]" text "<![endif]"))
          
          (not condition)
          (assoc :data text)))

(extend-protocol ToEnliveNode
  hamelito.parser.Element
  (-enlive-node [this] (element->enlive-node this))

  hamelito.parser.Text
  (-enlive-node [this] (:text this))

  hamelito.parser.Comment
  (-enlive-node [this] (comment->enlive-node this))
  
  hamelito.parser.Doctype
  (-enlive-node [this] (doctypes/lookup-doctype :html5 (:value this)))

  hamelito.parser.Document
  (-enlive-node [this]
    (concat (map -enlive-node (:doctypes this))
            (map -enlive-node (:elements this)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Public API

(defn node-seq
  [haml-source]
  (-> haml-source
      parser/parse-tree
      -enlive-node))

(defn parser
  [^java.io.InputStream stream]
  (with-open [^java.io.Closeable stream stream]
    (node-seq (io/reader stream))))

;; helpers for enlive pre 1.1.1, ie without support for pluggable
;; parsers.

(defn- ^java.io.InputStream resource-stream
  [path]
  (.. clojure.lang.RT baseLoader (getResourceAsStream path)))

(defn haml-resource
  [path]
  (with-open [stream (resource-stream path)]
    (into [] (node-seq (io/reader stream)))))

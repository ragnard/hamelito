(ns com.github.ragnard.hamelito.enlive
  (:require [com.github.ragnard.hamelito.doctypes :as doctypes]
            [com.github.ragnard.hamelito.parser   :as parser]
            [clojure.string    :as string]
            [clojure.java.io   :as io])
  (:import [com.github.ragnard.hamelito.parser
            Comment
            Document
            Doctype
            Element
            Text
            FilteredBlock]))

(when (< (:minor *clojure-version*) 5)
  (use 'com.github.ragnard.hamelito.util))

(def vec-conj (fnil conj []))

(defn flat-conj
  [vec xs]
  (apply vec-conj vec xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enlive Generation

(defprotocol ToEnliveNode
  (->enlive-node [this]))

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
          (update-in [:content] flat-conj (map ->enlive-node children))))

(defn- comment->enlive-node
  [{:keys [text condition children]}]
  (when children
    (throw (ex-info "Children to comments not yet supported" {})))
  (cond-> {:tag :comment}

          condition
          (assoc :data (str "[" condition "]" text "<![endif]"))
          
          (not condition)
          (assoc :data text)))

(defmulti filtered-block->enlive-node :type)

(defmethod filtered-block->enlive-node :plain
  [filtered-block]
  (apply str (interpose "\n" (:lines filtered-block))))

(defmethod filtered-block->enlive-node :javascript
  [filtered-block]
  {:tag :script :content (apply str (interpose "\n" (:lines filtered-block)))})

(defmethod filtered-block->enlive-node :css
  [filtered-block]
  {:tag :style :content (apply str (interpose "\n" (:lines filtered-block)))})

(defmethod filtered-block->enlive-node :default
  [filtered-block]
  (throw (ex-info (format "Unsupported filter type: %s" (:type filtered-block))
                  {:node filtered-block})))


(extend-protocol ToEnliveNode
  Element
  (->enlive-node [this] (element->enlive-node this))

  Text
  (->enlive-node [this] (:text this))

  Comment
  (->enlive-node [this] (comment->enlive-node this))

  FilteredBlock
  (->enlive-node [this] (filtered-block->enlive-node this))
  
  
  Doctype
  (->enlive-node [this] (doctypes/lookup-doctype (:type this) (:opts this)))

  Document
  (->enlive-node [this]
    (concat (map ->enlive-node (:doctypes this))
            (map ->enlive-node (:elements this)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Public API

(defn node-seq
  "Returns a seq of enlive nodes from the given haml-source. A
  haml-source is anything that satisfies the CharSeq protocol,
  typically a String or a Reader."
  [haml-source]
  (-> haml-source
      parser/parse-tree
      ->enlive-node))

(defn parser
  "A parser for Haml documents implementing the enlive pluggable
  parser interface"
  [^java.io.InputStream stream]
  (with-open [^java.io.Closeable stream stream]
    (node-seq (io/reader stream))))

;; helpers for enlive pre 1.1.1, ie. without support for pluggable
;; parsers.

(defn- ^java.io.InputStream resource-stream
  [path]
  (.. clojure.lang.RT baseLoader (getResourceAsStream path)))

(defn resource
  "Returns a seq of enlive nodes from Haml document at path on the
  classpath"
  [path]
  (with-open [stream (resource-stream path)]
    (into [] (node-seq (io/reader stream)))))

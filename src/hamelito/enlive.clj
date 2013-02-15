(ns hamelito.enlive
  (:require [hamelito.doctypes :as doctypes]
            [clojure.string  :as string])
  (:use [hamelito.parse-tree]))

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

(defn element->enlive-node
  [element]
  (let [{:keys [name id classes attributes inline-content children]} element]
    (cond-> {:tag (or (keyword name) :div)}

            id
            (assoc-in [:attrs :id] id)

            (not (empty? classes))
            (assoc-in [:attrs :class] (string/join " " classes))

            attributes
            (update-in [:attrs] merge attributes)

            inline-content
            (update-in [:content] vec-conj inline-content)

            children
            (update-in [:content] flat-conj (map -enlive-node children)))))

(extend-protocol ToEnliveNode
  hamelito.parse_tree.Element
  (-enlive-node [this] (element->enlive-node this))

  hamelito.parse_tree.Text
  (-enlive-node [this] (:text this))

  hamelito.parse_tree.Comment
  (-enlive-node [this] (concat ["<!--"]
                               (when (:text this) [(:text this)])
                               (mapv -enlive-node (:children this))
                               [" -->"]))
  
  hamelito.parse_tree.Doctype
  (-enlive-node [this] (doctypes/lookup-doctype :html5 (:value this)))

  hamelito.parse_tree.Document
  (-enlive-node [this]
    (concat (map -enlive-node (:doctypes this))
            (map -enlive-node (:elements this)))))

(defn node-seq
  [parse-tree]
  (-enlive-node parse-tree))

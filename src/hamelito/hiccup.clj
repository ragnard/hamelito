(ns hamelito.hiccup
  (:require [hamelito.doctypes :as doctypes]
            [hiccup.core       :as h]
            [hiccup.page       :as hp]
            [clojure.string    :as string])
  (:use [hamelito.parse-tree]))

;; bring in the cond->
(when (< (:minor *clojure-version*) 5)
  (use 'hamelito.util))

(defn- flat-conj
  [vec xs]
  (apply conj vec xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hiccup Conversion

(defprotocol ToHiccup
  (-to-hiccup [this]))

(defn element-tag
  [{:keys [name id classes]}]
  (keyword (str (or name "div")
                (when id
                  (str "#" id))
                (when-not (empty? classes)
                  (str "." (string/join "." classes))))))

(defn element->hiccup
  [{:keys [attributes inline-content children] :as element}]
  (cond-> [(element-tag element)]

          attributes
          (conj attributes)

          inline-content 
          (conj inline-content)

          children
          (flat-conj (mapv -to-hiccup children))))

(defn comment->hiccup
  [{:keys [text condition children]}]
  (concat ["<!--"]
          (when condition
            ["[" condition "]>"])
          (when text
            [text])
          (mapv -to-hiccup children)
          (when condition
            ["<![endif]"])
          ["-->"]))

(extend-protocol ToHiccup
  hamelito.parse_tree.Element
  (-to-hiccup [this] (element->hiccup this))

  hamelito.parse_tree.Text
  (-to-hiccup [this] (:text this))

  hamelito.parse_tree.Comment
  (-to-hiccup [this] (comment->hiccup this))
  
  hamelito.parse_tree.Doctype
  (-to-hiccup [this] (doctypes/lookup-doctype :html5 (:value this)))
  
  hamelito.parse_tree.Document
  (-to-hiccup [this] (concat
                      (mapv -to-hiccup (:doctypes this))
                      (mapv -to-hiccup (:elements this)))))

(defn to-hiccup
  [parse-tree]
  (-to-hiccup parse-tree))

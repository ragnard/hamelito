(ns com.github.ragnard.hamelito.hiccup
  (:require [com.github.ragnard.hamelito.header :as header]
            [com.github.ragnard.hamelito.parser :as parser]
            [hiccup.core                        :as hiccup]
            [clojure.string                     :as string])
  (:import [com.github.ragnard.hamelito.parser
            Comment
            Document
            Doctype
            Element
            Text
            FilteredBlock
            XmlProlog]))

(when (< (:minor *clojure-version*) 5)
  (use 'com.github.ragnard.hamelito.util))

(def vec-conj (fnil conj []))

(defn flat-conj
  [vec xs]
  (apply vec-conj vec xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hiccup Translation

(defprotocol ToHiccup
  (->hiccup [this]))

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

          (not (empty? attributes))
          (conj attributes)

          (not (string/blank? inline-content)) 
          (conj inline-content)

          children
          (flat-conj (mapv ->hiccup children))))

(defn comment->hiccup
  [{:keys [text condition children]}]
  (concat (if condition
            ["<!--[" condition "]>"]
            ["<!-- "])
          (when text
            [text])
          (mapv ->hiccup children)
          (if condition
            ["<![endif]-->"]
            [" -->"])))

(defmulti filtered-block->hiccup :type)

(defmethod filtered-block->hiccup :plain
  [filtered-block]
  (apply str (interpose "\n" (:lines filtered-block))))

(defmethod filtered-block->hiccup :javascript
  [filtered-block]
  [:script (apply str (interpose "\n" (:lines filtered-block)))])

(defmethod filtered-block->hiccup :css
  [filtered-block]
  [:style (apply str (interpose "\n" (:lines filtered-block)))])

(defmethod filtered-block->hiccup :cdata
  [filtered-block]
  (str "<![CDATA["
       (apply str (interpose "\n" (:lines filtered-block)))
       "]]>"))

(defmethod filtered-block->hiccup :default
  [filtered-block]
  (throw (ex-info (format "Unsupported filter type: %s" (:type filtered-block))
                  {:node filtered-block})))

(defn- doctype->hiccup
  [{:keys [name]}]
  (str "<!DOCTYPE "
       (apply str (header/lookup-doctype name))
       ">\n"))

(defn- xml-prolog->hiccup
  [{:keys [opts]}]
  (header/xml-prolog opts))

(extend-protocol ToHiccup
  Element
  (->hiccup [this] (element->hiccup this))

  Text
  (->hiccup [this] (:text this))

  FilteredBlock
  (->hiccup [this] (filtered-block->hiccup this))
  
  Comment
  (->hiccup [this] (comment->hiccup this))
  
  Doctype
  (->hiccup [this] (doctype->hiccup this))

  XmlProlog
  (->hiccup [this] (xml-prolog->hiccup this))
  
  Document
  (->hiccup [this] (concat (mapv ->hiccup (:header this))
                           (mapv ->hiccup (:elements this)))))

(defn- to-hiccup
  [parse-tree]
  (->hiccup parse-tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn hiccup
  "Returns hiccup data from the given haml-source. A haml-source is
  anything that satisfies the CharSeq protocol, typically a String or
  a Reader."
  [haml-source]
  (let [parse-tree (parser/parse-tree haml-source)]
    (to-hiccup parse-tree)))

(defn html
  "Returns a string with the result of rendering the Hiccup data
  generated by the hiccup function."
  [haml-source]
  (hiccup/html (seq (hiccup haml-source))))

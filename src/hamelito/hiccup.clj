(ns hamelito.hiccup
  (:require [hamelito.doctypes :as doctypes]
            [hamelito.parser   :as parser]
            [hiccup.core       :as hiccup]
            [clojure.string    :as string]))

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

          (not (string/blank? inline-content)) 
          (conj inline-content)

          children
          (flat-conj (mapv -to-hiccup children))))

(defn comment->hiccup
  [{:keys [text condition children]}]
  (concat (if condition
            ["<!--[" condition "]>"]
            ["<!-- "])
          (when text
            [text])
          (mapv -to-hiccup children)
          (if condition
            ["<![endif]-->"]
            [" -->"])))

(defmulti filtered-block->hiccup :type)

(defmethod filtered-block->hiccup :plaintext
  [filtered-block]
  (apply str (interpose "\n" (:lines filtered-block))))

(defmethod filtered-block->hiccup :javascript
  [filtered-block]
  [:script (apply str (interpose "\n" (:lines filtered-block)))])

(defmethod filtered-block->hiccup :default
  [filtered-block]
  (throw (ex-info (format "Unknown filter type: %s" (:type filtered-block))
                  {:node filtered-block})))

(extend-protocol ToHiccup
  hamelito.parser.Element
  (-to-hiccup [this] (element->hiccup this))

  hamelito.parser.Text
  (-to-hiccup [this] (:text this))

  hamelito.parser.FilteredBlock
  (-to-hiccup [this] (filtered-block->hiccup this))
  
  hamelito.parser.Comment
  (-to-hiccup [this] (comment->hiccup this))
  
  hamelito.parser.Doctype
  (-to-hiccup [this] (doctypes/lookup-doctype :html5 (:value this)))
  
  hamelito.parser.Document
  (-to-hiccup [this] (concat
                      (mapv -to-hiccup (:doctypes this))
                      (mapv -to-hiccup (:elements this)))))

(defn- to-hiccup
  [parse-tree]
  (-to-hiccup parse-tree))

(defn hiccup
  [haml-source]
  (let [parse-tree (parser/parse-tree haml-source)]
    (to-hiccup parse-tree)))

(defn html
  [haml-source]
  (hiccup/html (seq (hiccup haml-source))))

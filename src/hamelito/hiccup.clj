(ns hamelito.hiccup
  (:require [hiccup.core     :as h]
            [hiccup.page     :as hp]
            [clojure.string  :as string])
  (:use [hamelito.parse-tree]))

;; bring in cond-> if necessary
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

(def ^:private doctype-map
  {:default "<!DOCTYPE html>\n"
   "Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"})


(extend-protocol ToHiccup
  hamelito.parse_tree.Element
  (-to-hiccup [this]
    (let [{:keys [attributes inline-content children]} this]
      (cond-> [(element-tag this)]

              attributes
              (conj attributes)

              inline-content 
              (conj inline-content)

              children
              (flat-conj (mapv -to-hiccup children)))))

  hamelito.parse_tree.Text
  (-to-hiccup [this]  (:text this))

  hamelito.parse_tree.Doctype
  (-to-hiccup [this]  (get doctype-map (:value this) nil))
  
  hamelito.parse_tree.Document
  (-to-hiccup [this] (concat
                      (mapv -to-hiccup (:doctypes this))
                      (mapv -to-hiccup (:elements this)))))

(defn to-hiccup
  [parse-tree]
  (-to-hiccup parse-tree))

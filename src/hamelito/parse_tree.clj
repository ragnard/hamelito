(ns hamelito.parse-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parse Tree

;; node types

(defrecord HamlDocument [doctypes elements])
(defrecord Doctype [value])
(defrecord Text [text])
(defrecord Element [name id classes attributes text children])
(defrecord Comment [text condition children])


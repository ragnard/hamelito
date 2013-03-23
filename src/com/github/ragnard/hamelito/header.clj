(ns com.github.ragnard.hamelito.header
  (:require [com.github.ragnard.hamelito.config :as config]))

(defn xml-prolog
  [opts]
  (let [encoding (or (first opts) "utf-8")]
    (str "<?xml version='1.0' encoding='" encoding "' ?>")))

(def ^:private doctypes
  {::html5
   ["html"]

   ::xhtml-transitional
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML 1.0 Transitional//EN\""
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\""]

   ::xhtml-strict
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML 1.0 Strict//EN\""
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""]

   ::xhtml-frameset
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML 1.0 Frameset//EN\""
    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\""]

   ::xhtml-1.1
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML 1.1//EN\""
    "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""]

   ::xhtml-basic
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML Basic 1.1//EN\""
    "\"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\""]

   ::xhtml-mobile
   ["html"
    "PUBLIC"
    "\"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\""
    "\"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\""]

   ::xhtml-rdfa
   ["html"
    "PUBLIC"
    "\"-//W3C//DTD XHTML+RDFa 1.0//EN\""
    "\"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\""]})

(def ^:private format->doctypes
  {:xhtml
   {:default   ::xhtml-transitional

    "Strict"   ::xhtml-strict

    "Frameset" ::xhtml-frameset

    "1.1"      ::xhtml-1.1

    "Basic"    ::xhtml-basic

    "Mobile"   ::xhtml-mobile

    "RDFa"     ::xhtml-rdfa

    "5"        ::html5}

   :html5
   {:default   ::html5
    "5"        ::html5}})

(defn lookup-doctype
  [name]
  (let [doctype-key (get-in format->doctypes [(config/format) name])
        doctype (doctypes doctype-key)]
    (or doctype
        (throw (ex-info (format "Unknown doctype '%s' for format '%s'"
                                name (config/format))
                        {:doctype-name name
                         :haml-format  (config/format)})))))

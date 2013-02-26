(ns com.github.ragnard.hamelito.doctypes
  (:require [com.github.ragnard.hamelito.config :as config]))

(defn- xml
  [opts]
  (let [encoding (or (first opts) "utf-8")]
    (str "<?xml version='1.0' encoding='" encoding "' ?>")))

(def ^:private doctypes
  {:xhtml
   {:default
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

    "Strict"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

    "Frameset"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

    "5"
    "<!DOCTYPE html>"

    "1.1"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

    "Basic"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\">" 

    "Mobile"
    "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">"

    "RDFa"
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">"

    "XML"
    xml}

   :html5
   {:default
    "<!DOCTYPE html>\n"

    "XML"
    xml}})

(defn lookup-doctype
  [name opts]
  (let [doctype (get-in doctypes [(config/format) name])]
    (cond
     (fn? doctype)     (doctype opts)
     (string? doctype) doctype
     true              (throw (ex-info (format "Unknown doctype '%s' for format '%s'"
                                               name (config/format))
                                       {:doctype-name name
                                        :haml-format  (config/format)})))))

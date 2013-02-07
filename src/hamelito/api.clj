(ns hamelito.api
  (:require [hamelito.parsing     :as parsing]
            [hamelito.translation :as translation]
            [hiccup.core          :as hiccup]
            [clojure.java.io      :as io]))

(defprotocol CharSeq
  "things that can provide a seq of characters"
  (char-seq [this]))

(defn reader-char-seq
  [^java.io.Reader rdr]
  (let [c (.read rdr)]
    (when (not= -1 c)
      (cons (char c) (lazy-seq (reader-char-seq rdr))))))

(extend-protocol CharSeq
  String
  (char-seq [s] (seq s))

  java.io.Reader
  (char-seq [rdr] (reader-char-seq rdr)))

(defn hiccup
  [haml-source]
  (let [char-seq  (char-seq haml-source)
        parse-res (parsing/parse-haml char-seq)]
    (translation/to-hiccup parse-res)))

(defn html
  [haml-source]
  (let [hiccup-data (hiccup haml-source)]
    (hiccup/html (seq hiccup-data))))

(defn- ^java.io.InputStream resource-stream
  [path]
  (.. clojure.lang.RT baseLoader (getResourceAsStream path)))

(defn haml-resource
  [path]
  (with-open [stream (resource-stream path)]
    (let [html (html (io/reader stream))]
      (java.io.StringReader. html))))

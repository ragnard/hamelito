(ns hamelito.parsing
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as string]
            [hiccup.core    :as hiccup]
            [hiccup.compiler :as hiccup-compiler])
  (:use [blancas.kern.core]))

;;;; helpers

(defn quoted
  [char parser]
  (between (sym* char) (sym* char) parser))

(defn anything-but
  [char]
  (satisfy #(not= % char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HAML(ish) parser

;;;; general

(def identifier        (<|> alpha-num (sym* \-) (sym* \_)))

(def vspace            (many new-line*))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier))))

;;;; doctype
(def doctype-def       (token* "!!!" ))
(def doctype-value     (>> (sym* \space)
                           (<+> (many (anything-but \newline)))))
(def doctype           (bind [_     doctype-def
                              value (optional doctype-value)]
                             (return {:doctype (or value :default)})))
(def doctypes          (>> (optional vspace)
                           (sep-end-by vspace doctype)))


;;;; attributes

(defn quoted-any
  [char]
  (quoted char (<+> (many (anything-but char)))))

;; html-style attributes

(def html-name         (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def html-value        (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def html-attr-pair    (bind [_     (many white-space)
                              name  html-name
                              _     (sym* \=)
                              value html-value]
                             (return [name value])))

(def html-attr-sep     (many1 white-space))
(def html-attr-pairs   (sep-end-by html-attr-sep html-attr-pair))

(def open-paren        (sym* \())
(def close-paren       (sym* \)))
(def html-attributes   (between open-paren close-paren html-attr-pairs))

;; ruby-style attributes

(def ruby-keyword      (bind [_      (sym* \:)
                              name   (<+> (many1 identifier))]
                             (return (keyword name))))

(def ruby-name         (<|> ruby-keyword
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-value        (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-attr-pair    (bind [_      (many white-space)
                              name   ruby-name
                              _      (many white-space)
                              _      (token* "=>")
                              _      (many white-space)
                              value  ruby-value]
                             (return [name value])))

(def ruby-attr-sep     (<:> (<*> (many white-space)
                                 (sym* \,)
                                 (many white-space))))

(def ruby-attr-pairs   (<< (sep-by ruby-attr-sep ruby-attr-pair)
                           (many white-space)))

(def open-curly        (sym* \{))
(def close-curly       (sym* \}))
(def ruby-attributes   (between open-curly close-curly ruby-attr-pairs))

(def attributes        (<|> html-attributes ruby-attributes))


;; tag

(def element           (prefixed-identifier "%"))
(def id                (prefixed-identifier "#"))
(def class             (prefixed-identifier "."))

(def tag               (bind [el (optional element)
                              c1 (many class)
                              id (optional id)
                              c2 (many class)
                              at (optional attributes)
                              cl (optional (token* "/"))]
                             (return (when (or el
                                               id
                                               (not-empty c1)
                                               (not-empty c2))
                                       {:element el
                                        :id id
                                        :classes (into #{} (concat c1 c2))
                                        :self-close? (boolean cl)
                                        :attributes (into {} at)}))))

(def text              (<+> (many1 (anything-but \newline))))

(def inline-content    (>> (optional (sym* \space)) text))

(def indent            (<*> space space))

(def tag-line          (bind [level   (many indent)
                              t       tag
                              content (optional inline-content)]
                             (return {:level (count level)
                                      :tag t
                                      :content content})))

(def line              (>> vspace (<|> eof tag-line )))

(def lines             (many line))

(def haml              (bind [ds doctypes
                              ls lines]
                             (return {:doctypes ds
                                      :content ls})))

(comment

  (run* tag  "!!! XML\n\n%div.a.b.c#d42.e.f")

  (run* tag-line  "  %div.a.b.c#d42.e.f")

  (run* line  "\n\n  %div.a.b.c#d42.e.f\n    %p Hello World!")

  (run* lines  "\n\n  %div.a.b.c#d42.e.f\n\n\n\n    %p Hello World!\n\n")

  (run* haml  "%div/ Halo")

  (run* haml "%p(a='b'\n  c='d')")

  (run* haml "%div\n  %div\n    %p\n      text"  nil [:root {}])

  (pprint (parse-haml "%div{ :blah => 'honga'}\n  %div\n    %p\n      text\n\n"))

  )

(defn parse-haml
  [input]
  (parse haml input))

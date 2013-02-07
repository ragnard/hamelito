(ns hamelito.parsing
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as string])
  (:use [blancas.kern.core]
        [blancas.kern.lexer.basic]))

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

(def identifier2        (<|> alpha-num (sym* \-) (sym* \_)))

(def vspace            (many new-line*))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier2))))

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

(def html-name         (lexeme (<|> (<+> (many1 identifier2))
                                    (quoted-any \")
                                    (quoted-any \'))))

(def html-value        (lexeme (<|> (<+> (many1 identifier2))
                                    (quoted-any \")
                                    (quoted-any \'))))

(def html-attr-pair    (bind [name  html-name
                              _     (sym* \=)
                              value html-value]
                             (return [name value])))

(def html-attributes   (parens (many html-attr-pair)))


;; ruby-style attributes

(def ruby-keyword      (lexeme (bind [_      (sym \:)
                                      name   (<+> (many1 identifier2))]
                                     (return (keyword name)))))

(def ruby-name         (lexeme (<|> ruby-keyword
                                    (quoted-any \")
                                    (quoted-any \'))))

(def ruby-value        (lexeme (<|> (<+> (many1 identifier2))
                                    (quoted-any \")
                                    (quoted-any \'))))

(def ruby-attr-pair    (bind [name   ruby-name
                              _      (token "=>")
                              value  ruby-value]
                             (return [name value])))

(def ruby-attributes   (braces (comma-sep ruby-attr-pair)))

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

(defn- parse-succeded?
  [res]
  (and (:ok res)
       (nil? (seq (:input res)))))

(defn parse-haml
  [char-seq]
  (let [parse-res (parse haml char-seq)]
    (if (parse-succeded? parse-res)
      parse-res
      (throw (ex-info "HAML parsing failed"
                      {:parse-result parse-res})))))

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

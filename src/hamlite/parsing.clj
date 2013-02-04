(ns hamlite.parsing
  (:refer-clojure :exclude [class])
  (:use [blancas.kern.core]))

;;;; helpers

(defn quoted
  [char parser]
  (between (sym* char) (sym* char) parser))

(defn anything-but
  [char]
  (satisfy #(not= % char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HAML parser

;; doctype
(def doctype-prolog    (token* "!!!" ))
(def doctype-value     (many (anything-but \newline)))
(def doctype           (>> doctype-prolog doctype-value))
(def doctypes          (sep-by (many new-line*) doctype))

;; general

(def identifier (<|> alpha-num (sym* \-) (sym* \_)))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier))))


(def element           (prefixed-identifier "%"))
(def id                (prefixed-identifier "#"))
(def class             (prefixed-identifier "."))



;;;; attributes

(defn quoted-any
  [char]
  (quoted char (<+> (many (anything-but char)))))

;; html-style attributes

(def html-name         (<+> (many1 identifier)))
(def html-value        (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def html-attr-pair    (<*> html-name (sym* \=) html-value))

(def html-attr-pairs   (sep-by (many1 white-space) html-attr-pair))

(def open-paren        (sym* \())
(def close-paren       (sym* \)))
(def html-attributes   (between open-paren close-paren html-attr-pairs))

;; ruby-style attributes

(def ruby-keyword      (<+> (sym* \:) (many1 identifier)))

(def ruby-name         (<|> ruby-keyword
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-value        (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-attr-pair    (<*> ruby-name
                            (many white-space)
                            (token* "=>")
                            (many white-space)
                            ruby-value))

(def ruby-attr-sep     (<:> (<*> (many white-space) (sym* \,) (many white-space))))

(def ruby-attr-pairs   (sep-by ruby-attr-sep ruby-attr-pair))

(def open-curly        (sym* \{))
(def close-curly       (sym* \}))
(def ruby-attributes   (between open-curly close-curly
                                (<*> (many white-space)
                                     ruby-attr-pairs
                                     (many white-space))))

(def attributes        (<|> html-attributes ruby-attributes))


;; tag

(def tag               (bind [el (optional element)
                              c1 (many class)
                              id (optional id)
                              c2 (many class)
                              at (optional attributes)
                              cl (optional (token* "/"))]
                             (return {:element el
                                      :id id
                                      :classes (into #{} (concat c1 c2))
                                      :self-close? (boolean cl)
                                      :attributes at})))
                        
(def text              (<+> (many (anything-but \newline))))

(def inline-content    (>> (optional (sym* \space)) text))

(def indent            (<*> space space))

(def tag-line          (bind [level   (many indent)
                              t       tag
                              content inline-content]
                             (return {:level (count level)
                                      :tag t
                                      :content content})))
                        
(def vspace            (many new-line*))
                        
(def line              (>> vspace tag-line))
                        
(def lines             (many line))
                        
(def haml              (<< lines eof))
;; (def haml              (<< (<*> doctypes lines) eof))

;(run* element "%d")

(comment

  (run* tag  "!!! XML\n\n%div.a.b.c#d42.e.f")

  (run* tag-line  "  %div.a.b.c#d42.e.f")

  (run* line  "\n\n  %div.a.b.c#d42.e.f\n    %p Hello World!")

  (run* lines  "\n\n  %div.a.b.c#d42.e.f\n\n\n\n    %p Hello World!\n\n")

  (run* haml  "%div/ Halo")

  (run* haml "%p(a='b'\n  c='d')")

  (run* haml "%div\n  %div\n    %p\n      text")
  
  )

(defn parse-haml
  [input]
  (parse haml input))


(ns hamlite.parsing
  (:refer-clojure :exclude [class])
  (:use [blancas.kern.core]))

;;;; helpers

(defn quoted
  [char parser]
  (between (sym* char) (sym* char) parser))

(defn double-quoted
  [parser]
  (quoted \" parser))

(defn single-quoted
  [parser]
  (quoted \' parser))

(defn anything-but
  [char]
  (satisfy #(not= % char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HAML parser

(def identifier (<|> alpha-num (sym* \-) (sym* \_)))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier))))

(def element           (prefixed-identifier "%"))
(def id                (prefixed-identifier "#"))
(def class             (prefixed-identifier "."))

;; attributes

(defn quoted-any
  [char]
  (quoted char (<+> (many (anything-but char)))))

(def html-name         (<+> (many1 identifier)))
(def html-value        (<|> (<+> (many1 identifier))
                            (quoted-any \")
                            (quoted-any \')))

(def html-attr-pair    (<*> html-name (sym* \=) html-value))

(def html-attr-pairs   (sep-by (many1 white-space) html-attr-pair))

(def open-paren        (sym* \())
(def close-paren       (sym* \)))



(def html-attributes   (between open-paren close-paren html-attr-pairs))

(def attributes        html-attributes)

(def tag               (bind [el (optional element)
                              c1 (many class)
                              id (optional id)
                              c2 (many class)
                              at (optional attributes)
                              cl (optional (token* "/"))]
                             (return {:element el
                                      :id id
                                      :classes (into #{} (concat c1 c2))
                                      :self-close? (boolean cl)})))
                        
(def not-nl            (satisfy #(not= % \newline)))

(def text              (<+> (many not-nl)))

(def inline-content    (>> (optional (sym* \space)) text))

(def nested-content    text)

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

(println "------------------------------------------------------------------------")

;(run* element "%d")

(comment

  (run* tag  "%div.a.b.c#d42.e.f")

  (run* tag-line  "  %div.a.b.c#d42.e.f")

  (run* line  "\n\n  %div.a.b.c#d42.e.f\n    %p Hello World!")

  (run* lines  "\n\n  %div.a.b.c#d42.e.f\n\n\n\n    %p Hello World!\n\n")

  (run* haml  "%div/ Halo")

  (run* haml "%p(a='b'\n  c='d')")

  (pprint (run* haml "%div\n  %div\n    %p\n      text"))
  
  )

(defn parse-haml
  [input]
  (parse haml input))


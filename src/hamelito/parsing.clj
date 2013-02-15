(ns hamelito.parsing
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as string])
  (:use [hamelito.parse-tree]
        [blancas.kern.core]
        [blancas.kern.lexer.basic]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parse Tree

(def empty-document (->Document [] []))

(defprotocol ContentNode
  (update-children [this f args]))

(def vec-conj (fnil conj []))

(extend-protocol ContentNode
  hamelito.parse_tree.Element
  (update-children [this f args]
    (apply update-in this [:children] f args)))

(defn push-node
  [nodes level node]
  (if (< 0 level)
    (let [fixed  (butlast nodes)
          next   (last nodes)]
      (if-not (satisfies? ContentNode next)
        (throw (ex-info (format "Node '%s' does not support any content" (type next))
                        {:current node
                         :node next}))
        (conj (vec fixed)
              (update-children next push-node [(dec level) node]))))
    (vec-conj nodes node)))

(defn add-node
  [haml-document level node]
  (update-in haml-document [:elements] push-node level node))


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

(def vspace             (many new-line*))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier2))))

;;;; doctype
(def doctype-def       (token* "!!!" ))
(def doctype-value     (>> (sym* \space)
                           (<+> (many (anything-but \newline)))))
(def doctype           (bind [_     doctype-def
                              value (optional doctype-value)
                              _     (modify-state update-in [:doctypes] conj (map->Doctype {:value (or value :default)}))]
                             (return {:doctype (or value :default)})))
(def doctypes          (many (lexeme doctype)))

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


;;;; element

(def element           (prefixed-identifier "%"))
(def id                (prefixed-identifier "#"))
(def class             (prefixed-identifier "."))

(def text              (<+> (many (anything-but \newline))))

(def inline-content    (>> (sym* \space) text))

(def tag-element       (bind [el element
                              c1 (many class)
                              id (optional id)
                              c2 (many class)]
                             (return {:name el
                                      :id id
                                      :classes (into #{} (concat c1 c2))})))

(def class-element     (bind [c1 (many1 class)
                              id (optional id)
                              c2 (many class)]
                             (return {:id id
                                      :classes (into #{} (concat c1 c2))})))

(def id-element        (bind [id id
                              cl (many class)]
                             (return  {:id id
                                       :classes (into #{} cl)})))

(def tag-definition    (<|> tag-element class-element id-element))

(def tag               (bind [tag tag-definition
                              at  (optional attributes)
                              cl  (optional (token* "/"))
                              ic  (optional inline-content)]
                             (return (map->Element
                                      (merge tag
                                             {:attributes (into {} at)
                                              :self-close? (boolean cl)
                                              :inline-content ic})))))

(def comment-cond      (brackets (many1 identifier)))

(def html-comment      (bind [_      (sym \/)
                              cc     (optional comment-cond)
                              c      (optional text)]
                             (return (->Comment c cc))))

(def nested-content    (bind [text (<+> (anything-but \space)
                                        (many (anything-but \newline)))]
                             (return (map->Text {:text text}))))


(def indent            (<*> space space))

(def tag-line          (bind [level   (many indent)
                              node (<|> tag nested-content)
                              _       (modify-state add-node (count level) node)]
                             (return {:level (count level)
                                      :content node})))

(def line              (>> vspace (optional tag-line)))

(def lines             (many1 line))

(def haml              (bind [ds doctypes
                              ls lines]
                             (return {:doctypes ds
                                      :content ls})))

(def start             (>> trim (<< haml eof)))

(defn- parse-succeded?
  [res]
  (and (:ok res)
       (nil? (seq (:input res)))))

(defn parse-haml
  [char-seq]
  (let [parse-res (parse start char-seq nil empty-document)]
    (if (parse-succeded? parse-res)
      parse-res
      (throw (ex-info "HAML parsing failed"
                      {:parse-result parse-res})))))

(comment

  (run* tag  "!!! XML\n\n%div.a.b.c#d42.e.f")

  (run* tag-line  "  %div.a.b.c#d42.e.f")

  (run* line  "\n\n  %div.a.b.c#d42.e.f\n    %p Hello World!")

  (run* lines  "\n\n  %div.a.b.c#d42.e.f\n\n\n\n    %p Hello World!\n\n")

  (run* start  "%div/ Halo")

  (run* start "%p(a='b'\n  c='d')")

  (run* start "%div\n  %div\n    %p\n      text"  nil [:root {}])

  (pprint (parse-haml "%div{ :blah => 'honga'}\n  %div\n    %p\n      text\n\n"))

  )

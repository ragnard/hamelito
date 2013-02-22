(ns hamelito.parser
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as string])
  (:use [blancas.kern.core :exclude [char-seq]]))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parse Tree

(defrecord Document [doctypes elements])
(defrecord Doctype [value])
(defrecord Text [text])
(defrecord FilteredBlock [type lines])
(defrecord Element [name id classes attributes text children])
(defrecord Comment [text condition children])

;;;;--------------------------------------------------------------------
;;;; Parse tree construction

(def empty-document (->Document [] []))

(defprotocol ContentNode
  (update-children [this f args]))

(def vec-conj (fnil conj []))

(extend-protocol ContentNode
  Element
  (update-children [this f args]
    (apply update-in this [:children] f args))

  Comment
  (update-children [this f args]
    (apply update-in this [:children] f args)))

(defn- push-node
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HAML(ish) parser

;;;; Parser state functions

(defn- add-doctype
  [state doctype]
  (update-in state [:document :doctypes] conj doctype))

(defn- add-node
  [state level node]
  (update-in state [:document :elements] push-node level node))

(defn- set-indent
  [state indent]
  (assoc state :indent indent))

;;;; general parsers

(def identifier2        (<|> alpha-num (sym* \-) (sym* \_)))

(def vspace             (many new-line*))

(defn- anything-but
  [char]
  (satisfy #(not= % char)))

(defn- quoted
  [char parser]
  (between (sym* char) (sym* char) parser))

(defn- quoted-any
  [char]
  (quoted char (<+> (many (anything-but char)))))

(defn trim-ws
  [p]
  (<< p (many white-space)))

(defn trim-spaces
  [p]
  (<< p (many (one-of* (str \space \tab)))))

(defn parens*
  [p]
  (between (sym* \() (sym* \)) p))

(defn brackets*
  [p]
  (between (sym* \[) (sym* \]) p))

(defn braces*
  [p]
  (between (sym* \{) (sym* \}) p))

(defn prefixed-identifier
  [prefix]
  (>> (token* prefix)
      (<+> (many1 identifier2))))

(def indent            (<*> space space))

;;;;--------------------------------------------------------------------
;;;; doctype

(def doctype-def       (token* "!!!" ))
(def doctype-value     (>> (sym* \space)
                           (<+> (many (anything-but \newline)))))
(def doctype           (bind [_     doctype-def
                              value (optional doctype-value)
                              _     (modify-state add-doctype (map->Doctype {:value (or value :default)}))]
                             (return {:doctype (or value :default)})))
(def doctypes          (sep-end-by vspace doctype))

;;;;--------------------------------------------------------------------
;;;; attributes

;; html-style attributes

(def html-name         (<|> (<+> (many1 identifier2))
                            (quoted-any \")
                            (quoted-any \')))

(def html-value        (<|> (<+> (many1 identifier2))
                            (quoted-any \")
                            (quoted-any \')))

(def html-attr-pair    (bind [name  (trim-ws html-name)
                              _     (sym* \=)
                              value (trim-ws html-value)]
                             (return [name value])))

(def html-attributes   (parens* (skip-ws (many html-attr-pair))))

;; ruby-style attributes

(def ruby-keyword      (bind [_      (sym* \:)
                              name   (<+> (many1 identifier2))]
                             (return (keyword name))))

(def ruby-name         (<|> ruby-keyword
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-value        (<|> (<+> (many1 identifier2))
                            (quoted-any \")
                            (quoted-any \')))

(def ruby-attr-pair    (bind [name   (trim-ws ruby-name)
                              _      (trim-ws (token* "=>"))
                              value  (trim-ws ruby-value)]
                             (return [name value])))

(def ruby-attributes   (braces* (skip-ws (sep-by (trim-ws (sym* \,)) ruby-attr-pair))))

(def attributes        (<|> html-attributes ruby-attributes))


;;;;--------------------------------------------------------------------
;;;; element

(def element           (prefixed-identifier "%"))
(def id                (prefixed-identifier "#"))
(def class             (prefixed-identifier "."))

(def text              (<+> (many (anything-but \newline))))

(def inline-content    text)

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
                                              :inline-content (string/trim ic)})))))

;;;;--------------------------------------------------------------------
;;;; Comments

(def comment-cond      (brackets* (<+> (many (anything-but \])))))

(def html-comment      (bind [_         (sym* \/)
                              condition (optional comment-cond)
                              text      (optional text)]
                             (return (map->Comment
                                      {:text (string/trim text)
                                       :condition condition}))))

;;;;--------------------------------------------------------------------
;;;; Text Content

(def nested-content    (bind [text (<+> (anything-but \space)
                                        (many (anything-but \newline)))]
                             (return (map->Text
                                      {:text text}))))

;;;;--------------------------------------------------------------------
;;;; Filtered Content

(def filter-type       (bind [_    (sym* \:)
                              type (trim-spaces (<+> (many1 identifier2)))
                              _    (optional new-line*)]
                             (return (keyword type))))


(def consume-indent    (bind [usr    get-state]
                             (let [n (inc (:indent usr 0))]
                               (condp > n
                                 1 (return [])
                                 2 indent
                                (times n indent)))))

(def filter-line       (bind [_      (<:> consume-indent)
                              line   (<+> (many (anything-but \newline)))
                              _      (many vspace)]
                             (return line)))

(def filtered-content  (bind [type  filter-type
                              lines (many filter-line)]
                             (return (map->FilteredBlock
                                      {:type type
                                       :lines lines}))))

;;;;--------------------------------------------------------------------
;;; Main

(def tag-line          (bind [level (many indent)
                              _     (modify-state set-indent (count level))
                              node  (<|> tag
                                         html-comment
                                         filtered-content
                                         nested-content)
                              _     (modify-state add-node (count level) node)]
                             (return {:level (count level)
                                      :content node})))

(def lines             (sep-end-by vspace tag-line))

(def haml              (bind [ds doctypes
                              ls lines]
                             (return {:doctypes ds
                                      :content ls})))

(def start             (>> vspace (<< haml eof)))

(defn- parse-succeded?
  [pstate]
  (and (:ok pstate)
       (nil? (seq (:input pstate)))))

(defn- throw-error-message
  [{:keys [error] :as pstate}]
  (let [{:keys [line col]} (:pos error)
        msgs (#'blancas.kern.core/get-msg-list error)]
    (throw (ex-info (format "Parsing failed at line %s:%s: %s"
                            line col (string/join ", " msgs))
                    {:msgs msgs
                     :line line
                     :col col}))))

(defn- parse-haml
  [cs]
  (let [pstate (parse start (char-seq cs) nil {:document empty-document})]
    (if (parse-succeded? pstate)
      pstate
      (throw-error-message pstate))))

(defn parse-tree
  [char-seq]
  (-> (parse-haml char-seq) :user :document))

(comment

  (parse-tree " asldkasld")
  
  (run* tag  "!!! XML\n\n%div.a.b.c#d42.e.f")

  (run* tag-line  "  %div.a.b.c#d42.e.f")

  (run* line  "\n\n  %div.a.b.c#d42.e.f\n    %p Hello World!")

  (run* lines  "\n\n  %div.a.b.c#d42.e.f\n\n\n\n    %p Hello World!\n\n")

  (run* start  "%div/ Halo")

  (run* start "%p(a='b'\n  c='d')")

  (run* start "%div\n  %div\n    %p\n      text"  nil [:root {}])

  (pprint (parse-haml "%div{ :blah => 'honga'}\n  %div\n    %p\n      text\n\n"))

  )



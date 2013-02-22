# hamelito

[![Build Status](https://travis-ci.org/ragnard/hamelito.png?branch=master)](https://travis-ci.org/ragnard/hamelito)

> As the younger, distant cousin to [Haml](http://haml.info/), Hamelito
> looks up to his handsome relative, but has different goals in life.

hamelito is a [Clojure](http://www.clojure.org) library implementing a
parser for a subset of Haml, and the ability to generate hiccup,
enlive or html data. Parsing is done using the quite awesome parser
combinator library [kern](https://github.com/blancas/kern/) by Armando
Blancas.

The initial goal for the library is to allow Haml documents to be used
as templates for [enlive](http://github.com/cgrand/enlive) and the
plan is to support only the features of Haml that make sense for the
purpose of generating static documents, ie. there will be no support
for inline code.

## Status

Can parse a decent subset of Haml and generate corresponding hiccup
and enlive data.

Things that should work:
- Basic element content:
  - `%element`, `.class`, `#id` and valid combinations thereof
- Inline content:
  - `%h1 Hello`
- Text content
  - `Blahonga`
- Arbitrarily, valid, nested tags/content
- Attributes
  - Ruby style: `%a{:href => 'www.xkcd.org'}`
    - Single and double quoted values
  - HTML style: `%a(href='www.xkcd.org')`
    - Unquoted, single and double quoted values
- Comments
  - Html: `/ A haml comment`
  - Conditional: `/[if IE lte 10] No.`
- Filters
  - `:plain`
  - `:javascript`
  - `:cdata`
  - `:css`
- Doctypes 
  - Only `!!!` is currently supported an will yield HTML5 doctype

Hiccup to html translation [tests](https://github.com/ragnard/hamelito/blob/master/test/hamelito/hiccup_test.clj).

Things todo/investigate:
- Be more strict with valid identifiers when parsing
- Escaping
- Comments
  - Silent
- Whitespace handling/preservation
- More doctypes, especially !!! XML with encoding
- Filters?
  - Maybe these make sense:
    - `:escaped`

## Usage

Hamelito provides to main ways of consuming Haml documents:

1. As enlive nodes
2. As hiccup data

### enlive

Since version `1.1.1`, enlive supports pluggable parsers and Hamelito 
provides `hamelito.enlive/parser` which implements the required 
interface.

```clojure
(require '[net.cgrand.enlive-html :as enlive])
(require '[com.github.ragnard.hamelito.enlive :as haml])

(in-ns 'test)

;; To use Haml for all templates in the current namespace:

(enlive/set-ns-parser! haml/parser)

(deftemplate haml-template "template.haml"
  [])


;; To use Haml for a specific template/snippet:

(deftemplate haml-template {:parser haml/parser} "template.haml"
  [])
```

You can also ask for the raw enlive nodes:

```clojure
(require '[com.github.ragnard.hamelito.enlive :as [haml]])

(haml/node-seq "%h1 Blahonga!")
;; => ({:content ["Blahonga!"], :attrs {}, :tag :h1})
```

For versions of enlive prior to 1.1.1, a `resource` utility 
function is provided to simplify using HAML templates on the 
classpath with enlive:

```clojure
(require '[net.cgrand.enlive-html :as enlive])
(require '[hamelito.enlive :as [haml]])

(defsnippet snippet1 (haml/resource "page1.haml")
  [:body :h1] (enlive/content "Hola!"))

(deftemplate page1 (haml/resource "page1.haml")
  [:head :title] (enlive/content "Welcome!"))
```

### hiccup

```clojure
(require '[com.github.ragnard.hamelito.hiccup :as haml])

;; The hiccup function takes a Haml source and returns some 
;; hiccup data:

(haml/hiccup "%h1 Hola")
;; => ([:h1 "Hola"])

;; The html function takes a Haml source and returns an Html 
;; string, as generated by hiccup:

(haml/html "%h1 Hola!")
;; => "<h1>Hola!</h1>"
```

## License

Copyright © 2013 Ragnar Dahlen

Distributed under the Eclipse Public License, the same as Clojure.

(ns helmer.rendering-test
  (:require [helmer.rendering :as r])
  (:use [clojure.test]))

(defmacro defhtmltest
  [name & test-cases]
  `(deftest ~name
     (are [input# expected#] (= expected# (r/to-html input#))
          ~@test-cases)))

(defhtmltest basic

  ""
  ""

  "A"
  "A"

  "foo bar"
  "foo bar"

  "%div"
  "<div></div>"

  "%div\n%div"
  "<div></div><div></div>"

  "\n\n%div"
  "<div></div>"

  "%div\n\n"
  "<div></div>"

  "%p"
  "<p />"                               ; hiccup auto-closes

  "%p Foo"
  "<p>Foo</p>"

  "%p Foo Bar"
  "<p>Foo Bar</p>")

(defhtmltest elements
  "%x"
  "<x />"

  "#a"
  "<div id=\"a\"></div>"

  ".a"
  "<div class=\"a\"></div>"

  ".a.b"
  "<div class=\"a b\"></div>"

  "#a.b.c"
  "<div class=\"b c\" id=\"a\"></div>"

  "%x#a.b.c"
  "<x class=\"b c\" id=\"a\" />"

  "%x.a.b#c.d.e"
  "<x class=\"a b d e\" id=\"c\" />"

  "%x.a.b#c.d.e Blahonga"
  "<x class=\"a b d e\" id=\"c\">Blahonga</x>"
  
  )

(defhtmltest nested-elements

  "%div\n  %div"
  "<div><div></div></div>"

  "%div Hello There\n  %div Stranger!"
  "<div>Hello There<div>Stranger!</div></div>"

  "%div\n  %div\n%div"
  "<div><div></div></div><div></div>"

  "%div Hello There\n  %div Stranger!\n%div More Stuff"
  "<div>Hello There<div>Stranger!</div></div><div>More Stuff</div>"


  
  )

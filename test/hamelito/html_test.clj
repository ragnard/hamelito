(ns hamelito.html-test
  (:require [hamelito.api :as hamelito])
  (:use [clojure.test]))

(defmacro defhtmltest
  [name & test-cases]
  `(deftest ~name
     (are [input# expected#] (= expected# (hamelito/html input#))
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
  "<div>Hello There<div>Stranger!</div></div><div>More Stuff</div>")

(defhtmltest nested-content

  "%div The quick\n  %p Brown\n    Fox\n  Jumped\n"
  "<div>The quick<p>BrownFox</p>Jumped</div>"

  )

(defhtmltest html-attributes

  "%div()"
  "<div></div>"

  "%div()"
  "<div></div>"

  "%div(abc='def')"
  "<div abc=\"def\"></div>"

  "%div(abc=\"def\")"
  "<div abc=\"def\"></div>"

  "%div('abc'=\"def\")"
  "<div abc=\"def\"></div>"

  "%div(\"abc\"=\"def\")"
  "<div abc=\"def\"></div>"

  "%div(  abc='def')"
  "<div abc=\"def\"></div>"

  "%div(abc='def' )"
  "<div abc=\"def\"></div>"

  "%div(  abc='def'   )"
  "<div abc=\"def\"></div>"

  "%div(   abc='def'  cde='fgh'   )"
  "<div abc=\"def\" cde=\"fgh\"></div>"

  "%div(   \nabc='def'\n  cde='fgh'\n\n   )"
  "<div abc=\"def\" cde=\"fgh\"></div>"

  )

(defhtmltest ruby-attributes

  "%div{}"
  "<div></div>"

  "%div{:abc=>'def'}"
  "<div abc=\"def\"></div>"

  "%div{:abc  =>  'def'}"
  "<div abc=\"def\"></div>"

  "%div{  :abc => 'def'}"
  "<div abc=\"def\"></div>"

  "%div{:abc => 'def' }"
  "<div abc=\"def\"></div>"

  "%div{   :abc=>'def' ,  :cde=>'fgh'   }"
  "<div abc=\"def\" cde=\"fgh\"></div>"

  "%div{   :abc=>'def'\n , \n :cde=>'fgh' \n\n  }"
  "<div abc=\"def\" cde=\"fgh\"></div>"
  )

(defhtmltest doctypes

  "!!!"
  "<!DOCTYPE html>\n"

  "\n\n!!!\n\n"
  "<!DOCTYPE html>\n"
  
  "!!!\n%div Blahonga"
  "<!DOCTYPE html>\n<div>Blahonga</div>"

  "\n\n!!!\n\n%div Blahonga\n\n"
  "<!DOCTYPE html>\n<div>Blahonga</div>"
  
  )

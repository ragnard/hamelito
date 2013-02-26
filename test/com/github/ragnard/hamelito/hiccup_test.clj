(ns com.github.ragnard.hamelito.hiccup-test
  (:require [com.github.ragnard.hamelito.hiccup :as hiccup]
            [com.github.ragnard.hamelito.config :as config])
  (:use [clojure.test]))

(defmacro defhtmltest
  [name & test-cases]
  `(deftest ~name
     (are [input# expected#] (= expected# (hiccup/html input#))
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

  "%div(a=b) Hello"
  "<div a=\"b\">Hello</div>"
  
  "%div(a=b)\n  %div(c=d)"
  "<div a=\"b\"><div c=\"d\"></div></div>"
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

  "%div{:a=>\"b\"} Hello"
  "<div a=\"b\">Hello</div>"
  
  "%div{:a=>\"b\"}\n  %div(c=d)"
  "<div a=\"b\"><div c=\"d\"></div></div>")

(defhtmltest doctypes

  "!!!"
  "<!DOCTYPE html>\n"

  "\n\n!!!\n\n"
  "<!DOCTYPE html>\n"
  
  "!!!\n%div Blahonga"
  "<!DOCTYPE html>\n<div>Blahonga</div>"

  "\n\n!!!\n\n%div Blahonga\n\n"
  "<!DOCTYPE html>\n<div>Blahonga</div>"

  "!!! XML\n\n!!!\n\n%div Blahonga\n\n"
  "<?xml version='1.0' encoding='utf-8' ?><!DOCTYPE html>\n<div>Blahonga</div>"

  "!!! XML iso-8859-1\n\n!!!\n\n%div Blahonga\n\n"
  "<?xml version='1.0' encoding='iso-8859-1' ?><!DOCTYPE html>\n<div>Blahonga</div>"
  )

(defhtmltest html-comments

  "/ This is a comment"
  "<!-- This is a comment -->"

  "\n\n/ This is a comment   \n\n"
  "<!-- This is a comment -->"
  
  "%div\n  / comment\n  Content"
  "<div><!-- comment -->Content</div>"

  "/\n  %h1 Nested\n    %p Content"
  "<!-- <h1>Nested<p>Content</p></h1> -->"
  )

(defhtmltest conditional-comments

  "/[if IE lte 10]"
  "<!--[if IE lte 10]><![endif]-->"
  
  "/[if IE]\n  %p Go away\n"
  "<!--[if IE]><p>Go away</p><![endif]-->"
  
  )

(defhtmltest plain-block

  ":plain"
  ""

  ":plain\n  a"
  "a"

  ":plain\n   a"
  " a"

  ":plain\n  a b c  "
  "a b c  "

  ":plain\n  a\n  b\n  c"
  "a\nb\nc"

  ":plain\n  a\n   b  \n    c  "
  "a\n b  \n  c  "

  ":plain\n  a\n   b  \n    c  \n"
  "a\n b  \n  c  "

  "%div\n  :plain\n    a"
  "<div>a</div>"

  "%div\n  :plain\n     a"
  "<div> a</div>"

  "%div\n  :plain\n    a\n\n    b"
  "<div>a\nb</div>"

  "%div One\n  :plain\n    Hello\n\n%div Two"
  "<div>OneHello</div><div>Two</div>"

  "%div One\n  %div Two\n    :plain\n      Three\n      Four\n    %div Five"
  "<div>One<div>TwoThree\nFour<div>Five</div></div></div>"
  )

(defhtmltest javascript-block

  ":javascript"
  "<script></script>"

  ":javascript\n  alert('hello world!');"
  "<script>alert('hello world!');</script>"

  )

(defhtmltest css-block
  ":css"
  "<style></style>"

  ":css\n  font-weight: bold;"
  "<style>font-weight: bold;</style>"

  ":css\n  font-weight: bold;\n  color: brown;"
  "<style>font-weight: bold;\ncolor: brown;</style>"

  )

(defhtmltest cdata-block

  ":cdata"
  "<![CDATA[]]>"
  
  ":cdata\n  content"
  "<![CDATA[content]]>"

  ":cdata\n  one  \n   two "
  "<![CDATA[one  \n two ]]>"
  
  )

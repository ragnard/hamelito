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

  ;; "%div\n\n"
  ;; "<div></div>"

  "%p"
  "<p />"

  "%p Foo"
  "<p>Foo</p>"

  "%p Foo Bar"
  "<p>Foo Bar</p>")

(defhtmltest nested-elements

  "%div\n  %div"
  "<div><div></div></div>"

  "%div Hello There\n  %div Stranger!"
  "<div>Hello There<div>Stranger!</div></div>"

  "%div\n  %div\n%div"
  "<div><div></div></div><div></div>"

  "%div Hello There\n  %div Stranger!\n%div More Stuff"
  "<div>Hello There<div>Stranger!</div></div><div>More Stuff</div>")

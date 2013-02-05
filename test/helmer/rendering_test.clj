(ns helmer.rendering-test
  (:require [helmer.rendering :as r])
  (:use [clojure.test]))

(defmacro rendering-test
  [name & test-cases]
  `(deftest ~name
     (are [input# expected#] (= expected# (r/to-html input#))
          ~@test-cases)))


(rendering-test basic
                
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

(rendering-test nested-elements
                
                "%div\n  %div"
                "<div><div></div></div>"

                "%div\n  %div\n%div"
                "<div><div></div></div><div></div>"
                
                
                )

(ns com.github.ragnard.hamelito.util)

(defmacro cond->
  "Conditional threading macro. Takes an initial value and an even
  number of steps that are pairs of predicate and form to be threaded
  if pred is true"
  [init & steps]
  (assert (even? (count steps)))
  (let [g (gensym)
        pstep (fn [[pred step]] `(if ~pred (-> ~g ~step) ~g))]
    `(let [~g ~init
           ~@(interleave (repeat g) (map pstep (partition 2 steps)))]
       ~g)))

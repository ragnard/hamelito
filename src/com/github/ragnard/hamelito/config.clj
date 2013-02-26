(ns com.github.ragnard.hamelito.config
  (:refer-clojure :exclude [format]))

(def default-config
  {:format :html5})

(def ^:dynamic *config* default-config)

(defn format
  []
  (:format *config*))

(defmacro with-config
  [config & body]
  `(binding [*config* ~config]
     ~@body))

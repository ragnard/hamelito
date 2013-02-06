(ns hamelito.rendering
  (:require [clojure.string :as string]
            [hiccup.compiler :as hc]
            [hamelito.parsing :as p]))

(def vec-conj (fnil conj []))

(defn hiccup-tag
  [{:keys [element id classes]}]
  (keyword (str (or element "div")
                (when id
                  (str "#" id))
                (when-not (empty? classes)
                  (str "." (string/join "." classes))))))

(defn tag-data->hiccup
  [{:keys [tag content]}]
  (if tag
    (reduce (fn [res val]
              (if val
                (conj res val)
                res))
            [(hiccup-tag tag)]
            [(or (:attributes tag) {})
             content])
    content))


(defn push-content
  [[tag attrs & body :as data] level new]
  (if (< 0 level)
    (conj (vec (concat [tag attrs]
                       (butlast body)))
          (push-content (last body) (dec level) new))
    (vec-conj data new)))


(defn- hepp
  [input]
  (let [parse-res (p/parse-haml input)]
    (reduce (fn [res {:keys [level tag content] :as tag-data}]
              (if tag-data
                (push-content res level (tag-data->hiccup tag-data))
                res))
            [:root {}]
            (:lines (:value parse-res)))))

(defn to-hiccup
  [input]
  (hepp input))

(defn to-html
  [input]
  (let [res (hepp input)]
    (clojure.pprint/pprint res)
    (eval (apply hc/compile-html (drop 2 res )))))

(comment

  (-> [:root {}]
      (update-level 0 [:html {}]))
  
  (pprint  (-> [:root {}]
               (update-level 0 [:html {}])
               (update-level 1 [:head {}])
               (update-level 1 [:body {}])
               (update-level 2 [:h1 {} "Title 1"])
               (update-level 3 [:div {} ])
               (update-level 3 [:p {}  "Paragraph"])
               (update-level 2 [:h1 {}  "Title 2"])
               (update-level 0 "Heeej")))
  )






















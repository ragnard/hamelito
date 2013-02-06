(ns hamelito.hiccup
  (:require [clojure.string :as string]
            [hiccup.core     :as h]
            [hiccup.compiler :as hc]
            [hiccup.page     :as hp]
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

[:root {}]

[:root {} [:div] [:div]]

[]

(defn push-content
  [data level new]
  (if (< 0 level)
    (let [fixed  (butlast data)
          next   (last data)]
      (if (vector? next)
        (conj (vec fixed)
              (push-content next (dec level) new))
        (throw (ex-info "No parent for content" {:content new
                                                 :state data
                                                 :level level}))))
    (vec-conj data new)))


(defn- reduce-lines
  [parse-res]
  (reduce (fn [res {:keys [level tag content] :as tag-data}]
            (if tag-data
              (push-content res level (tag-data->hiccup tag-data))
              res))
          []
          (:lines (:value parse-res))))

(defn to-hiccup
  [input]
  (let [parse-res (p/parse-haml input)]
    (reduce-lines parse-res)))

(defn to-html
  [input]
  (let [hiccup (to-hiccup input)]
    (clojure.pprint/pprint hiccup)
    (h/html (seq hiccup))))

(comment

  (-> []
      (push-content 0 [:html]))

  (-> []
      (push-content 0 [:a])
      (push-content 0 [:b]))

  (-> []
      (push-content 0 [:a])
      (push-content 1 [:b {}]))

  (pprint  (-> []
               (push-content 0 [:html])
               (push-content 1 [:head])
               (push-content 1 [:body])
               (push-content 2 [:h1 "Title 1"])
               (push-content 3 [:div {:class "gurka"}])
               (push-content 3 [:p "Paragraph"])
               (push-content 2 [:h1 {}  "Title 2"])
               (push-content 4 [:h1 {}  "Title 2"])
               (push-content 0 "Heeej")))
  )

(ns helmer.rendering
  (:require [clojure.string :as string]
            [hiccup.compiler :as hc]
            [helmer.parsing :as p]))


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

(def vec-conj (fnil conj []))

(defn update-level
  [[tag attrs & body :as data] level new]
  (if (< 0 level)
    (conj (vec (concat [tag attrs]
                       (butlast body)))
          (update-level (last body) (dec level) new))
    (vec-conj data new)))


(defn- hepp
  [input]
  (let [ast (p/parse-haml input)]
    (reduce (fn [res {:keys [level tag content] :as tag-data}]
              (if tag-data
                (update-level res level (tag-data->hiccup tag-data))
                res))
            [:root {}]
            (:value ast))))

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


  
  [[:html {}
    [:head {}]
    [:body {}
     [:h1 {} "Title 1"
      [:div {}]
      [:p {} "Paragraph"]]
     [:h1 {} "Title 2"]]]]


  '{:indent 0
    :tag "div"}


  [:div {} _]

  [tag attrs & body]


  [:div {} nil]

  (defn get-level
    [data level]
    (nth (iterate last data) level))

  (defn vec-update-in
    [v [k & ks] f & args]
    (if ks
      (vec-assoc v k (apply vec-update-in (get v k) ks f args))
      (vec-assoc v k (apply f (get v k) args))))
  
  
  (def d [:html {}
          [:head]
          [:body
           [:h1 "Hello"]
           [:p "Paragraph"]]])

  

  ;;
  ;; - Find level (must exist)
  ;; - conj
  ;;

  )






















(ns hamelito.enlive
  (:require [clojure.string  :as string]))

(when (< (:minor *clojure-version*) 5)
  (use 'hamelito.util))

(def vec-conj (fnil conj []))

(defn tag->enlive-node
  [{:keys [element id classes attributes]} content]
  (cond-> {:tag (or (keyword element) :div)}

          id
          (assoc-in [:attrs :id] id)

          (not (empty? classes))
          (assoc-in [:attrs :class] (string/join " " classes))

          attributes
          (update-in [:attrs] merge attributes)

          content
          (assoc :content (if content [content] []))))

(defn tag-data->enlive-node
  [{:keys [tag content]}]
  (if tag
    (tag->enlive-node tag content)
    content))

(defn push-content
  [node-seq level content]
  (if (< 0 level)
    (let [fixed  (butlast node-seq)
          next   (last node-seq)]
      (if (map? next)
        (conj (vec fixed)
              (update-in next [:content]
                         push-content (dec level) content))
        (throw (ex-info "Missing parent for new element or content"
                        {:content content
                         :node-seq node-seq
                         :level level}))))
    (vec-conj node-seq content)))

(defn- content->node-seq
  [{:keys [content]}]
  (reduce (fn [res {:keys [level tag content] :as tag-data}]
            (if tag-data
              (push-content res level (tag-data->enlive-node tag-data))
              res))
          []
          content))


(defn node-seq
  [parse-res]
  (let [value (:value parse-res)]
    (concat
     ;; TODO: doctype
     (content->node-seq value))))


(comment

  (-> []
      (push-content 0 {:tag :html
                       :attrs {:a 1}
                       :content ["Hello"]}))

  (-> []
      (push-content 0 {:tag :a})
      (push-content 0 {:tag :b}))

  (-> []
      (push-content 0 {:tag :a})
      (push-content 1 {:tag :b}))

  (-> []
      (push-content 0 {:tag :a})
      (push-content 1 {:tag :b})
      (push-content 1 "asdf"))


  (pprint  (-> []
               (push-content 0 [:html])
               (push-content 1 [:head])
               (push-content 1 [:body])
               (push-content 2 [:h1 "Title 1"])
               (push-content 3 [:div {:class "gurka"}])
               (push-content 3 [:p "Paragraph"])
               (push-content 2 [:h1 {}  "Title 2"])
               (push-content 3 [:h1 {}  "Title 2"])
               (push-content 0 "Heeej")))

  
  )

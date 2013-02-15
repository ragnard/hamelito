(ns hamelito.hiccup
  (:require [hiccup.core     :as h]
            [hiccup.page     :as hp]
            [clojure.string  :as string]))

(when (< (:minor *clojure-version*) 5)
  (use 'hamelito.util))

(def vec-conj (fnil conj []))

(defn tag->hiccup
  [{:keys [element id classes]}]
  (keyword (str (or element "div")
                (when id
                  (str "#" id))
                (when-not (empty? classes)
                  (str "." (string/join "." classes))))))

(defn tag-data->hiccup
  [data]
  (cond
   (map? data)    (let [attributes (:attributes data)
                       content     (:inline-content data)]
                   (cond-> [(tag->hiccup data)]

                           attributes
                           (conj attributes)

                           content
                           (conj content)))
   (string? data) data))

(defn apply-content
  [hiccup-data level content]
  (if (< 0 level)
    (let [fixed  (butlast hiccup-data)
          next   (last hiccup-data)]
      (if (vector? next)
        (conj (vec fixed)
              (apply-content next (dec level) content))
        (throw (ex-info "Missing parent for new element or content"
                        {:content content
                         :hiccup-data hiccup-data
                         :level level}))))
    (vec-conj hiccup-data content)))

(defn- content->hiccup
  [{:keys [content]}]
  (reduce (fn [hiccup-data {:keys [level content] :as data}]
            (if data
              (apply-content hiccup-data level (tag-data->hiccup content))
              hiccup-data))
          []
          content))


(def ^:private doctype-map
  {:default "<!DOCTYPE html>\n"
   "Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"})


(defn- doctype->hiccup
  [{:keys [doctypes]}]
  (for [{:keys [doctype]} doctypes]
    (get doctype-map doctype nil)))

(defn to-hiccup
  [parse-res]
  (let [value (:value parse-res)]
    (concat
     (doctype->hiccup value)
     (content->hiccup value))))


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
               (push-content 3 [:h1 {}  "Title 2"])
               (push-content 0 "Heeej")))
  )

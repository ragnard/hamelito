(ns hamelito.hiccup
  (:require [clojure.string :as string]
            [hiccup.core     :as h]
            [hiccup.compiler :as hc]
            [hiccup.page     :as hp]
            [hamelito.parsing :as p]))

(defprotocol CharSeq
  "things that can be seqs of characters"
  (char-seq [this]))

(defn- reader-char-seq
  [rdr]
  (let [c (.read rdr)]
    (when (not= -1 c)
      (cons (char c) (lazy-seq (reader-char-seq rdr))))))

(extend-protocol CharSeq
  String
  (char-seq [s] (seq s))

  java.io.Reader
  (char-seq [rdr] (reader-char-seq rdr)))

(def vec-conj (fnil conj []))


(defn tag->hiccup
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
            [(tag->hiccup tag)]
            [(:attributes tag) content])
    content))

(defn push-content
  [data level content]
  (if (< 0 level)
    (let [fixed  (butlast data)
          next   (last data)]
      (if (vector? next)
        (conj (vec fixed)
              (push-content next (dec level) content))
        (throw (ex-info "Missing parent for new element or content"
                        {:content content
                         :state data
                         :level level}))))
    (vec-conj data content)))

(defn- content->hiccup
  [{:keys [content]}]
  (reduce (fn [res {:keys [level tag content] :as tag-data}]
            (if tag-data
              (push-content res level (tag-data->hiccup tag-data))
              res))
          []
          content))

(def ^:private doctype-map
  {:default "<!DOCTYPE html>\n"
   "Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"})

(defn- doctype->hiccup
  [{:keys [doctypes]}]
  (for [{:keys [doctype]} doctypes]
    (get doctype-map doctype nil)))

(defn- to-hiccup
  [source]
  (let [char-seq  (char-seq source)
        parse-res (p/parse-haml char-seq)
        value     (:value parse-res)]
    (concat
     (doctype->hiccup value)
     (content->hiccup value))))

(defn to-html
  [source]
  (let [hiccup (to-hiccup source)]
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
               (push-content 3 [:h1 {}  "Title 2"])
               (push-content 0 "Heeej")))
  )

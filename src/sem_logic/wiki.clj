(ns sem-logic.wiki
  (:require [clojure.data.json :as json]))


(defn wiktionary-page [word]
  (-> "https://en.wiktionary.org/w/api.php?action=query"
      (str "&format=json&prop=revisions&rvprop=content")
      (str "&titles=" word)
      slurp
      json/read-str))

(defn wikipedia-page [word]
  (-> "https://en.wikipedia.org/w/api.php?action=query"
      (str "&format=json&prop=revisions|categories&rvprop=content")
      (str "&titles=" word)
      slurp
      json/read-str))

(defn wikicategories [page]
  (-> page
      (get "query")
      (get "pages")
      vals
      first
      (get "categories")))

(defn wikitext [word-page]
  (-> word-page
      (get "query")
      (get "pages")
      vals
      first
      (get "revisions")
      first
      (get "*")))

(defn transitivity [wikitext]
  (let [contexts (->> wikitext
                      (re-seq #"\{\{.+\}\}")
                      (filter #(re-find #"context" %)))
        all (count (filter #(re-find #"transitive" %) contexts))
        trans (count (filter #(re-find #"[^n]transitive" %) contexts))]
    (cond (and (> all 0) (> trans 0)) :transitive
          (> all 0) :intransitive)))

#_(transitivity "{{context|intransitive}}")


(defn animals [wikitext]
  (count (re-seq #"# An?[^\.:,]+animal" wikitext)))

(defn persons [wikitext]
  (count (re-seq #"# An?[^\.:,]+person" wikitext)))



(defmulti from-wiktionary (fn [type word text] type))

(defmethod from-wiktionary 'NN
  [type word text]
  (assoc (cond (> (animals text)
                  (persons text) {:type :animal})
               (< (animals text)
                  (persons text) {:type :person}))
    :pred (symbol word)))

(defmethod from-wiktionary 'VBZ
  [type word text]
  (let [trans (transitivity text)]
    (assoc (cond trans {:transitivity trans})
      :pred (symbol word))))

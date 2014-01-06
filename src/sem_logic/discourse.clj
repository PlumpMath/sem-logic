(ns sem-logic.discourse
  (:refer-clojure :exclude [==])
  (:use sem-logic.wiki
        clojure.core.ogic
        clojure.core.logic.pldb
        opennlp.nlp
        opennlp.treebank)
  (:require [clojure.core.logic.fd :as fd]))


(def treebank-parser
  (make-treebank-parser "parser-model/en-parser-chunking.bin"))

(def name-find (make-name-finder "parser-model/en-ner-person.bin"))

(def lexicon (atom {}))

(defn find-lexicon [type word]
  (let [entry (@lexicon word)]
    (if entry entry
        (let [page (wiktionary-page word)
              text (wikitext page)]
          (get (swap! lexicon assoc word
                      (if text
                        (with-meta (from-wiktionary type word text)
                          {:wiki page})
                        {})) word)))))

(defn parse-sentence [sent]
  (-> (treebank-parser [sent])
      first
      make-tree))

;; analyzers

(defmulti analyze "Analyze chunks." :tag)

;; non-terminal

(defmethod analyze 'TOP
  [elem]
  (update-in elem [:chunk] #(map analyze %)))

(defmethod analyze 'S
  [elem]
  (let [new-elem (update-in elem [:chunk] #(mapv analyze %))]
    (if (second new-elem)
      (assoc-in new-elem [:chunk 0 :role] :subject))))

(defmethod analyze 'NP
  [elem]
  (update-in elem [:chunk] #(map analyze %)))

(defmethod analyze 'VP
  [elem]
  (let [new-elem (update-in elem [:chunk] #(mapv analyze %))]
    (if (second new-elem)
      (assoc-in new-elem [:chunk 1 :role] :object))))

;; word-level, terminal

(defmethod analyze 'VBZ
  [{[chunk] :chunk :as elem}]
  (let [third-sing (.endsWith chunk "s")
        chunk (if third-sing
                (subs chunk 0 (dec (.length chunk)))
                chunk)
        new-elem (merge elem
                        (find-lexicon 'VBZ chunk)
                        {:role :verb})]
    (if third-sing (assoc new-elem :quantity 1)
        new-elem)))

(defmethod analyze 'PRP
  [{[chunk] :chunk :as elem}]
  (apply assoc elem
         (case (.toLowerCase chunk)
           "i" [:quantity 1 :determined true :type :person]
           "you" [:determined true :type :person]
           "he" [:quantity 1 :determined true :genus :male :type :person]
           "she" [:quantity 1 :determined true :genus :female :type :person]
           "it" [:quantity 1 :determined true :genus :neutrum :type :animal]
           "them" [:quantity :many :determined true])))

(defmethod analyze 'DT
  [{[chunk] :chunk :as elem}]
  (apply assoc elem
         (case (.toLowerCase chunk)
           "a" [:quantity 1 :determined false]
           "all" [:quantity :all :determined false]
           "none" [:quantity 0 :determined false]
           "the" [:determined true]
           "this" [:quantity 1 :determined true]
           "these" [:quantity :many :determined true]
           "that" [:quantity 1 :determined true])))

(defmethod analyze 'NN
  [{[chunk] :chunk :as elem}]
  (let [chunk (if (.endsWith chunk "s")
                (subs chunk 0 (dec (.length chunk)))
                chunk)]
    (merge elem (find-lexicon 'NN chunk))))

(defmethod analyze :default
  [elem]
  (assoc elem :not-supported true))

;; nouns
;; morphology -> singular, plural, genus?
;; name?
;; lexicon -> genus

;; verb
;; tempus, modus


;; target: temporal, modal, intensional annotations

(defmulti emit :tag)

;; non-terminal

(defmethod emit 'TOP
  [{chunk :chunk :as elem}]
  (apply merge (map emit chunk)))

(defmethod emit 'S
  [{chunk :chunk :as elem}]
  (let [[subj] (filter #(= (:role %) :subject) (map emit chunk))
        [{:keys [verb object]}] (filter :verb (map emit chunk))]
    {:subject subj
     :verb verb
     :object object}))

(defmethod emit 'NP
  [{chunk :chunk :as elem}]
  (merge (apply merge (map emit chunk)) elem))


(defmethod emit 'VP
  [{chunk :chunk :as elem}]
  (let [[verb] (filter #(= (:role %) :verb) chunk)
        [obj] (filter #(= (:role %) :object) chunk)]
    {:verb verb
     :object obj}))

;; word-level, terminal

(defmethod emit :default
  [elem]
  elem)





(comment

  (name-find ["Johns"])

  (transitivity (-> (meta (@lexicon "own"))
                  :wiki
                  wikitext))



  (swap! lexicon (fn [old] {}))

  #_(emit (analyze (parse-sentence "A farmer owns a donkey .")))
  #_(analyze (parse-sentence "He beats it ."))

  (analyze {:tag 'DT
          :chunk '("A")})



  (db-rel Noun name var)
  (db-rel TransVerb name subj obj)
  (db-rel Verb name subj obj)

  (def model (atom (db)))

  (swap! model db-fact TransVerb 'own 'farmer 'donkey)

  (def emission (emit (analyze (parse-sentence "A farmer owns a donkey ."))))

  (defn magic [{:keys [subject verb object]}]
  (run* [q]
        (== q (case (verb :transitivity)
                :transitive 42))))

  (magic emission))

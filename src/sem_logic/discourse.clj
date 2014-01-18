(ns sem-logic.discourse
  (:refer-clojure :exclude [==])
  (:use sem-logic.wiki
        clojure.core.logic
        clojure.core.logic.pldb
        opennlp.nlp
        opennlp.treebank)
  (:require [clojure.core.logic.fd :as fd]))


(def treebank-parser
  (make-treebank-parser "parser-model/en-parser-chunking.bin"))

(def name-find
  (make-name-finder "parser-model/en-ner-person.bin"))

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


(defmulti drs
  "Generate a discourse representative structure
  from a parse tree recursively."
  (fn [chunk _ _] (:tag chunk)))


(defmethod drs 'TOP
  [{[S] :chunk} down up]
  (drs S down up))


(defmethod drs 'S
  [{[a b c d e f] :chunk} down up]
  (matche [(:tag a) (:tag b) (:tag c)]
          [['NP 'VP _]
           ;; indefinite nominal phrase
           (fresh [?np ?vp ?subj ?verb ?obj ?univ ?conds ?conds-a]
                  (fresh [?univ-a]
                         (featurec ?np {:univ ?univ-a :conds ?conds-a})
                         (featurec ?vp {:univ ?univ :conds ?conds})
                         (drs a down ?np)
                         (project [?univ-a ?conds-a]
                                  (drs b {:univ ?univ-a
                                          :conds ?conds-a} ?vp)))
                  ;; merge into new drs
                  (featurec ?vp {:verb ?verb})
                  (conda [(featurec ?vp {:object ?obj})]
                         [(== ?obj nil)])
                  (project [?univ ?conds ?np ?obj]
                           (== up {:univ ?univ
                                   :conds (conj ?conds
                                                (if ?obj
                                                  [?verb
                                                   (:id ?np)
                                                   (:id ?obj)]
                                                  [?verb (:id ?np)]))})))]))



(defmethod drs 'NP
  [{[a b c d e f] :chunk} down up]
  (fresh [?defined]
         (membero ?defined [true false])
         (project [down ?defined]
                  (matche [(:tag a) (:tag b)]
                          [['DT 'NN]
                           (all (drs a down up)
                                (drs b (assoc down :defined ?defined) up))]
                          [['PRP _] (drs a down up)]))))


(defmethod drs 'PRP
  [{[prp] :chunk} down up]
  (let [pro (case (.toLowerCase prp)
              "it" {:quant 1 :gender :neutrum :defined true}
              "she" {:quant 1 :gender :female :defined true}
              "he" {:quant 1 :gender :male :defined true}
              "him" {:quant 1 :gender :male :defined true})
        id (java.util.UUID/randomUUID)
        pro (assoc pro :id id)]
    (== up (merge down
                  pro
                  {:univ (conj (:univ down) pro)}))))


(defn- find-verb [verb]
  (if (.endsWith verb "s")
    {:verb (find-lexicon 'VBZ (subs verb
                                    0
                                    (dec (count verb))))
     :quant 1}
    {:verb (find-lexicon 'VBZ verb)}))


(defmethod drs 'VP
  [{[a b c d e f] :chunk} down up]
  (let [verb (find-verb (-> a :chunk first))]
    (matche [(:tag a) (:tag b)]

            [['VBZ _]
             (!= (:tag b) 'NP)
             (== up (merge down verb))]

            [['VBZ 'NP]
             (fresh [?np]
                    (drs b down ?np)
                    (project [?np]
                             (== up
                                 (merge down
                                        {:univ (:univ ?np)}
                                        verb
                                        {:object ?np}))))])))


(defmethod drs 'DT
  [{[det] :chunk} down up]
  (case (.toLowerCase det)
    "a" (featurec up {:defined false})
    "an" (featurec up {:defined false})
    "the" (featurec up {:defined true})))


(defmethod drs 'NN
  [{[noun] :chunk} down up]
  (let [id (java.util.UUID/randomUUID)
        n (assoc (find-lexicon 'NN noun) :id id)]
    (== up (merge down
                  {:univ (conj (:univ down) n)}
                  n))))

(defmethod drs 'NNP
  [{[noun] :chunk} down up]
  (let [nom {:id (java.util.UUID/randomUUID)
             :pred noun}]
    (== up (merge down
                  {:univ (conj (:univ down) nom)}
                  nom ))))

(reduce (fn [model sent]
          (first (run 1 [q] (drs (parse-sentence sent) model q))))
        {:univ []
         :conds []}
        ["A sheep sleeps ."])










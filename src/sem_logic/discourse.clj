(ns sem-logic.discourse
  (:refer-clojure :exclude [==])
  (:use sem-logic.wiki
        clojure.core.logic
        clojure.core.logic.pldb
        opennlp.nlp
        opennlp.treebank)
  (:require [clojure.core.logic.fd :as fd]))


(def treebank-parser (make-treebank-parser "parser-model/en-parser-chunking.bin"))

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


(defmulti ->drs
  "Generate a discourse representative structure from a parse tree recursively."
  (fn [chunk _ _] (:tag chunk)))


(defmethod ->drs 'TOP
  [{[S] :chunk} down up]
  (->drs S down up))


(defmethod ->drs 'S
  [{[a b c d e f] :chunk} down up]
  (matche [(:tag a) (:tag b) (:tag c)]
          [['NP 'VP _]
           ;; indefinite nominal phrase
           (fresh [?np ?vp ?verb ?obj]
                  (->drs a {:defined false} ?np)
                  (->drs b {} ?vp)
                  (featurec ?vp {:object ?obj})
                  (featurec ?vp {:verb ?verb})
                  (== up (merge-with concat down {:universe [?np ?obj]
                                                  :conditions [[?verb ?np ?obj]]})))]))


(defmethod ->drs 'NP
  [{[a b c d e f] :chunk} down up]
  (project [down]
           (matche [(:tag a) (:tag b)]
                   [['DT 'NN]
                    (all (->drs a down up)
                         (->drs b down up))]
                   [['PRP _] (->drs a down up)])))


(defmethod ->drs 'PRP
  [{[prp] :chunk} down up]
  (== up (merge down (case (.toLowerCase prp)
                       "it" {:quant 1 :gender :neutrum :defined true}
                       "him" {:quant 1 :gender :male :defined true}))))


(defn- find-verb [verb]
  (if (.endsWith verb "s")
    {:verb (find-lexicon 'VBZ (subs verb
                                    0
                                    (dec (count verb))))
     :quant 1}
    {:verb (find-lexicon 'VBZ verb)}))


(defmethod ->drs 'VP
  [{[a b c d e f] :chunk} down up]
  (let [verb (find-verb (-> a :chunk first))]
    (matche [(:tag a) (:tag b)]

            [['VBZ _]
             (== up (merge down {:verb verb}))]

            [['VBZ 'NP]
             (fresh [?np ?defined]
                    (membero ?defined [true false])
                    (->drs b {:defined ?defined} ?np)
                    (project [?np] (== up (merge down verb {:object ?np}))))])))


(defmethod ->drs 'DT
  [{[det] :chunk} down up]
  (case (.toLowerCase det)
    "a" (featurec up {:defined false})
    "the" (featurec up {:defined true})))


(defmethod ->drs 'NN
  [{[noun] :chunk} down up]
  (== up (merge down {:noun (find-lexicon 'NN noun)})))


(reduce (fn [drs sent] (first (run 1 [q] (->drs (parse-sentence sent) drs q))))
        {}
        ["A donkey owns a farmer ."
         "It beats him ."])

(ns sem-logic.parser
  (:require [clojure.java.io :as io]
            [opennlp.nlp :as onlp]
            [opennlp.treebank :as treebank])
  (:import [edu.berkeley.nlp.PCFGLA Parser
          Grammar Lexicon ParserData CoarseToFineMaxRuleParser TreeAnnotations]
          [edu.berkeley.nlp.syntax Tree]
          [edu.berkeley.nlp.util Numberer]))



(def pdata
  (-> (io/resource "eng_sm6.gr")
      io/as-file
      .getAbsolutePath
      ParserData/Load))

;; some hints on setup from mochii and berkeley sources
(def berk-parser
  (do (-> pdata
          .getNumbs
          Numberer/setNumberers)
    (CoarseToFineMaxRuleParser.
     (.getGrammar pdata)
     (.getLexicon pdata)
     1.0 -1 false false false false false true true)))

(def opennlp-parser
  (treebank/make-treebank-parser "parser-model/en-parser-chunking.bin"))

(def name-find
  (onlp/make-name-finder "parser-model/en-ner-person.bin"))

(def tokenize (onlp/make-tokenizer "parser-model/en-token.bin"))

(defn vectorize [nested-seqs]
  (if (seq? nested-seqs)
    (mapv vectorize (filter #(not (and (seq? %) (empty? %))) nested-seqs))
    nested-seqs))

(defn opennlp-sentence [sent]
  (-> (opennlp-parser [sent])
      first
      read-string
      vectorize))

(defn berkeley-sentence [sent]
  (-> berk-parser
      (.getBestParse (tokenize sent))
      (TreeAnnotations/unAnnotateTree false)
      .toString
      read-string
      vectorize))




(comment
  (berkeley-sentence "A farmer sleeps long.")
)
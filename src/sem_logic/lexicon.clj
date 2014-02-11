(ns sem-logic.lexicon
  (:require [sem-logic.wiki :as wiki]))


(def lexicon  (atom {})
  #_(atom {"own" {:transitivity :transitive :pred "own"}
                    "beat" {:transitivity :transitive :pred "beat"}
                    "farmer" {:gender :male :pred "farmer"}
                    "donkey" {:gender :neutrum :pred "donkey"}}))

(defn find-lexicon [type word]
  (let [entry (@lexicon word)]
    (if entry entry
        (let [page (wiki/wiktionary-page word)
              text (wiki/wikitext page)]
          (get (swap! lexicon assoc word
                      (if text
                        (with-meta (wiki/from-wiktionary type word text)
                          {:wiki page})
                        {})) word)))))

(def irregular
  {"am" {:verb "be"}
   "are" {:verb "be"}
   "is" {:verb "be" :quant 1}})

(defn find-verb [verb]
  (cond
   (contains? irregular verb) (irregular verb)
   (.endsWith verb "s")
    {:verb (find-lexicon 'VBZ (subs verb
                                    0
                                    (dec (count verb))))
     :quant 1}
    :else {:verb (find-lexicon 'VBZ verb)}))


(defn term
  "Return terminal (word) of node."
  [node]
  (-> node second name))

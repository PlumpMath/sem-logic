(ns sem-logic.discourse
  (:refer-clojure :exclude [==])
  (:use [clojure.core.match :refer [match]]
        clojure.core.logic
        clojure.core.logic.pldb)
  (:require [sem-logic.lexicon :refer [find-verb find-lexicon term]]
            [sem-logic.parser :refer [opennlp-sentence berkeley-sentence]]
            [clojure.core.logic.fd :as fd]))

(defn ^:dynamic *id-fn* [] (java.util.UUID/randomUUID))

(defn node-dispatch [[f] _] f)

(defmulti drs node-dispatch)

(defmethod drs 'ROOT
  [[_ a b c d] down]
  (match [a b]
         [['INTJ] _] (drs a down)
         [['S & _] _] (drs a down)))

(defn partial-sent [{:keys [univ conds filler]} np-chunk vp-chunk]
  (let [np (drs np-chunk {:univ univ :conds conds})
        subj (dissoc np :univ :conds)
        {np-conds :conds np-univ :univ} np

        {vp-univ :univ vp-conds :conds verb :verb obj :object neg :negation}
        (drs vp-chunk {:univ np-univ :conds np-conds})

        obj (or obj filler)
        neg (or neg false)]
    {:univ vp-univ
     :conds (conj (or vp-conds [])
                  (if obj [verb (:id subj) (:id obj)]
                    [verb (:id subj)]))
     :neg neg}))


(defmethod drs 'S
  [[_ a b c d] {:keys [univ conds] :as down}]
  (match [a b c]
         [['S & _] ['CC disjun] ['S & _]]
         {:univ univ :conds conds
          (keyword disjun) #{(drs a {}) (drs c {})}
          :neg false}

         [['NP [(:or 'DT 'JJ)
                (:or 'Every 'every 'All 'all 'Some 'some 'Many 'many 'Few 'few 'None)] & _]
          ['VP & _] _]
         (let [{:keys [univ conds neg]} (partial-sent down a b) ]
           {:univ [] :conds []
            :ante {:univ [(first univ)]
                   :conds []
                   :neg false}
            :quant (.toLowerCase (name (get-in a [1 1])))
            :conseq {:univ (rest univ)
                     :conds conds
                     :neg neg}
            :neg false})

         ;; indefinite nominal phrase
         [['NP & _] ['VP & _] _]
           (partial-sent down a b)

         ;; conditional phrase
         [['SBAR ['IN & _] ['S & _]] ['NP & _] ['VP & _]]
         {:univ univ :conds conds
          :ante (assoc (drs (a 2) {:univ [] :conds []}) :neg false)
          :conseq (partial-sent {} b c)
          :neg false}

         ;; subject gap
         [['VP & _] _ _]
         (let [{:keys [univ conds negation]} (drs a down)]
           {:univ univ :conds conds
            :filler (down :filler)
            :neg negation})))



(defmethod drs 'NP
  [[_ a b c d] down]
  (match [a b]
         [['NP & _] ['SBAR ['WHNP & _] rel-phrase]]
         (let [entity (drs a down)
               rel (drs rel-phrase (assoc entity :filler entity))]
           (assoc entity
             :univ (:univ rel)
             :conds (:conds rel)))

         [[(:or 'DT 'JJ) _] [(:or 'NN 'NNS) _]] (->> down (drs a) (drs b))
         [['NNP & _] _] (drs a down)
         [['PRP & _] _] (drs a down)) )


(defmethod drs 'PRP
  [[_ prp] down]
  (let [pro (case (.toLowerCase (name prp))
              "it" {:quant 1 :gender :neutrum :defined true}
              "she" {:quant 1 :gender :female :defined true}
              "he" {:quant 1 :gender :male :defined true}
              "him" {:quant 1 :gender :male :defined true})
        id (*id-fn*)
        pro (assoc pro :id id)]
    (merge down
           pro
           {:univ (conj (or (:univ down) []) pro)})))


(defmethod drs 'DT
  [[_ det] down]
  (case (.toLowerCase (name det))
    "every" down
    "all" down
    "some" down
    "a" (merge down {:defined false})
    "an" (merge down {:defined false})
    "the" (merge down {:defined true})))

(defmethod drs 'JJ
  [[_ det] down]
  down)


(defmethod drs 'NN
  [[_ noun] down]
  (let [id (*id-fn*)
        n (assoc (find-lexicon 'NN (name noun)) :id id)]
    (merge down
           {:univ (conj (or (:univ down) []) n)}
           n)))

(defmethod drs 'NNS
  [[_ noun] down]
  (let [id (*id-fn*)
        n (assoc (find-lexicon 'NNS (name noun)) :id id)]
    (merge down
           {:univ (conj (or (:univ down) []) n)}
           n)))


(defmethod drs 'NNP
  [[_ noun] down]
  (let [nom {:id (*id-fn*)
             :pred (name noun)
             :quant 1
             :type :name}]
    (merge down
           {:univ (conj (or (:univ down) []) nom)}
           nom)))


(defmethod drs 'VP
  [[_ a b c d] down]
  (let [verb (find-verb (term a))]
    (match [a b c]

           [[(:or 'VBZ 'VBP 'VB) & _] ['RB _] ['VP & _]]
            (assoc (drs c down) :negation true)

           [[(:or 'VBZ 'VBP 'VB) & _] ['NP & _] _]
           (let [obj (drs b down)]
             (merge down
                    verb
                    {:object obj}
                    {:univ (:univ obj)
                     :conds (:conds obj)}))

           [[(:or 'VBZ 'VBP 'VB) & _] _ _] (merge down verb))))

(comment
  (reduce (fn [model sent]
            (drs (berkeley-sentence sent) model))
            {:univ []
             :conds []}
            ["A farmer owns a donkey."
             "He beats it."])
  )


(ns sem-logic.core-test
  (:require [clojure.test :refer :all]
            [sem-logic.discourse :refer [drs]]
            [sem-logic.parser :refer [berkeley-sentence]]))



(defn test-sentence [sent]
  (let [symbols (atom '[a b c d e f g h i j k l m n o p q r s t u v w x y z])]
    (binding [sem-logic.discourse/*id-fn* #(first (swap! symbols rest))]
       (drs (berkeley-sentence sent) {:univ [] :conds []}))))


(deftest nominal-phrase-test
  (testing "Simple nominal phrases."
    (is (= (test-sentence "A farmer owns a donkey.")
           '{:univ [{:id b, :quant 1, :pred "farmer"}
                    {:id c, :quant 1, :pred "donkey"}],
             :conds [[{:pred "own"} b c]]
             :neg false}))
    (is (= (test-sentence "He beats it.")
           '{:univ [{:id b, :gender :male, :quant 1, :defined true}
                    {:id c, :gender :neutrum, :quant 1, :defined true}],
             :conds [[{:pred "beat"} b c]]
             :neg false}))))


;; conditional clauses
;; [S [if [S beta] (then) [s gamma]]]
;; 1. remove alpha from C_K
;; 2. duplex condition: add K_1 => K_2 to C_K
;; K_1 = [ | beta]            antecendens
;; K_2 = [ | gamma]           consequence
(deftest conditional-test
  (testing "Test for conditional rule."
    (is (= (test-sentence "If a farmer owns a donkey he beats it.")
           '{:univ [], :conds [],
            :ante {:univ [{:id b, :quant 1, :pred "farmer"}
                          {:id c, :quant 1, :pred "donkey"}],
                   :conds [[{:pred "own"} b c]]
                   :neg false},
            :conseq {:univ [{:id d, :quant 1, :gender :male, :defined true}
                            {:id e, :quant 1, :gender :neutrum, :defined true}],
                     :conds [[{:pred "beat"} d e]]
                     :neg false}
            :neg false}))
    (is (= (test-sentence "If a professor owns a book, he gives it to a student.")
           '{:univ [], :conds [],
             :ante {:univ [{:id b, :quant 1, :pred "professor"}
                           {:id c, :quant 1, :pred "book"}],
                    :conds [[{:pred "own"} b c]]
                    :neg false},
             :conseq {:univ [{:id d, :quant 1, :gender :male, :defined true}
                             {:id e, :quant 1, :gender :neutrum, :defined true}],
                      :conds [[{:pred "give"} d e]]
                      :neg false}
             :neg false}))))




;; relative clauses
;; [N [N beta] [RC [RPRO gamma] delta]]
;; 1. remove alpha(x) from C_K
;; 2. add beta(x) to C_K
;; 3. add delta to C_K after NP-gap has been filled
(deftest rel-test
  (testing "Test for relative clause with object gap."
    (is (= (test-sentence "Jones owns a book which Smith adores.")
           '{:univ [{:id b, :pred "Jones", :quant 1, :type :name}
                    {:id c, :quant 1, :pred "book"}
                    {:id d, :pred "Smith", :quant 1, :type :name}],
             :conds [[{:pred "adore"} d c] [{:pred "own"} b c]]
             :neg false}))))



;; universal nominal phrase
;; [S [NP beta] [VP gamma]] or [VP [V gamma] [NP beta]]
;; beta = every/all
;; 1. remove alpha(x) from C_K
;; 2. add K_1 => K_2 where
;;   K_1 = [ x | delta(x) (predicate) ]
;;   K_2 = [ | alpha']
;; alpha' is alpha with beta replaced by x
(deftest universal-np-test
  (testing "Testing universal nominal phrase as implication."
    (is (= (test-sentence "All farmers own a donkey.")
           '{:univ [], :conds [],
             :ante {:univ [{:id b, :pred "farmer", :quant :many}],
                    :conds []
                    :neg false},
             :conseq {:univ [{:id c, :quant 1, :pred "donkey"}],
                      :conds [[{:pred "own"} b c]]
                      :neg false}
             :neg false}))))


(deftest negation-test
  (testing "Testing negational sentence."
    (= (test-sentence "He doesn't read it.")
       '{:univ [{:id b, :quant 1, :gender :male, :defined true}
                {:id c, :quant 1, :gender :neutrum, :defined true}],
         :conds [[{:pred "read"} b c]],
         :neg true})))


;; sentence level disjunction
;; alpha in form of [S [S beta] or [S gamma]]
;; 1. Remove alpha from C_K
;; 2. Add K_1 disjunction K_2
;;   where K_1 = [ | beta ], K_2 = [ | gamma]
(deftest sentence-level-disjunction
  (testing "Sentence level disjunction."
    (is (= (test-sentence "He loves it or he hates it.")
           '{:univ [], :conds [],
             :or #{{:univ [{:id d, :quant 1, :gender :male, :defined true}
                           {:id e, :quant 1, :gender :neutrum, :defined true}],
                    :conds [[{:pred "love"} d e]],
                    :neg false}
                   {:univ [{:id b, :quant 1, :gender :male, :defined true}
                           {:id c, :quant 1, :gender :neutrum, :defined true}],
                    :conds [[{:pred "hate"} b c]],
                    :neg false}},
             :neg false}))))


;; universal nominal phrases
;; [S [NP beta] [VP gamma]] or [VP [V gamma] [NP beta]]
(comment)
(run-tests)


(ns sem-logic.core
   (:refer-clojure :exclude [==])
   (:use clojure.core.logic
         clojure.core.logic.pldb)
   (:require [clojure.core.logic.fd :as fd]))


;; Time Predicats
(defn F [pred x n]
  (fresh [times fut]
         (T times)
         (membero fut times)
         (fd/> fut n)
         (pred x fut)))

(defn P [pred x n]
  (fresh [times past]
         (T times)
         (membero past times)
         (fd/< past n)
         (pred x past)))

(defn H [pred x n]
  (project [n] (everyg #(pred x %) (range 1 n))))

(defn G [pred x n]
  (fresh [time]
         (T time)
         (project [n time]
                  (everyg #(pred x %)
                          (range n (inc (apply max time)))))))


;; TODO
;; - import words into universe
;; - map NP and VP to predicates
;;   - lift CC VPs
;; - predicate logic quantifiers
;; - linguistic quantifiers

(db-rel T time)
(db-rel D name t)

(db-rel Pr president t)
(db-rel Sl sleep t)

(def hans-maria-model
  (db [T [1 2 3]]
      [D ['p 'h 'm]]

      [Pr 'h 1]
      [Pr 'h 2]
      [Pr 'p 3]

      [Sl 'p 1]
      [Sl 'p 3]

      [Sl 'h 1]
      [Sl 'h 2]

      [Sl 'm 1]
      [Sl 'm 2]))

;; forall x : P(x) \rightarrow P(p*)
(with-db hans-maria-model
  (doall (run* [t]
               (fresh [times dom]
                      (T times)
                      (D dom)
                      (membero t times)
                      (everyg  #(conda [(Pr % t) (Pr 'p t)]
                                       [s#])
                               dom)))))


;; forall x : PS(x) \rightarrow HS(x)
(with-db hans-maria-model
  (doall (run* [now]
               (fresh [dom time]
                      (T time)
                      (D dom)
                      (membero now time)
                      (everyg #(conda [(P Sl % now)
                                       (H Sl % now)]
                                      [s#])
                              dom)))))

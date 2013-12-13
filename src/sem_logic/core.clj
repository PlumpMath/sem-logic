(ns sem-logic.core
   (:refer-clojure :exclude [==])
   (:use clojure.core.logic
         clojure.core.logic.pldb)
   (:require [clojure.core.logic.fd :as fd]))


(db-rel T time)
(db-rel U name)

(db-rel Pr president t)
(db-rel Sl sleep t)


;; Time Predicats, more elegance welcome ;-)
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
         (project [n]
                  (everyg #(pred x %)
                          (range n (inc (apply max time)))))))

(def M
  (db [T [1 2 3]] ;; order-rel is fd/<
      [U ['p 'h 'm]]

      ;; discrete representation of V
      [Pr 'h 1]
      [Pr 'h 2]
      [Pr 'p 3]

      [Sl 'p 1]
      [Sl 'p 3]

      [Sl 'h 1]
      [Sl 'h 2]

      [Sl 'm 1]
      [Sl 'm 2]))

(defn implies [P Q]
  (conda [(P) (Q)]
         [s#]))

(defn time [t]
  (fresh [times]
         (T times)
         (membero t times)))

;; forall x : P(x) \rightarrow P(p*)
(with-db M
  (run* [a t]
        (time t)
        (U a)
        (everyg (fn [x] (implies #(Pr x t)
                                #(Pr 'p t)))
                a)))


;; forall x : PS(x) \rightarrow HS(x)
(with-db M
  (run* [a t]
        (time t)
        (U a)
        (everyg (fn [x] (implies #(P Sl x t)
                                #(H Sl x t)))
                a)))

;; FPS(h*)
(with-db (db [T [1 2 3]]
             [U ['h]]

             [Sl 'h 2])
  (run* [x t]
        (time t)
        (fresh [dom]
               (U dom)
               (membero x dom))
        (F (partial P Sl) x t)))

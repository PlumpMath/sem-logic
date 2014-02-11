(ns sem-logic.bot
  (:require [sem-logic.discourse :refer [drs *id-fn*]]
            [sem-logic.parser :refer [berkeley-sentence get-sentences]]
            [irclj.core :as irc]
            [clojure.pprint :refer [pprint]]))

;; TODO
;; - store predicates
;; - remove predicates

(def my-nick "sem-logic-bot")


(defn drs-all-sentences [s]
  (let [symbols (atom '[a b c d e f g h i j k l m n o p q r s t u v w x y z])]
    (map (fn [sent]
           (binding [sem-logic.discourse/*id-fn* #(first (swap! symbols rest))]
             (try (drs (berkeley-sentence sent) {:univ [] :conds []})
                  (catch Exception e
                    e))))
         (get-sentences s))))



(defn- pp-str [edn] ;; HACK pprint, find proper way?
  (let [out (java.io.StringWriter.)]
    (binding [*out* out]
      (pprint edn)
      (map second (re-seq #"(.+)\n" (.toString out))))))


(declare connection)
(defn ^:dynamic privmsg [ref m]
  (println "privmsg: " m)
  (let [[_ msg] (re-seq (re-pattern (str "\\s*" my-nick ".?\\s*(.+)"))
                        (:text m))]
    (when (and (not= "sem-logic-bot" (:nick m)) msg)
      (doseq [line (pp-str (first (drs-all-sentences msg)))]
        (irc/reply connection m line)))))


(def connection (irc/connect "kornbluth.freenode.net"
                             7000
                             "sem-logic-bot"
                             :ssl? true
                             :callbacks {:privmsg privmsg}))

(comment

(irc/join connection "##sem-logic")

(irc/message connection "sem-logic-bot" "hello world!")

(irc/part connection "#votorola" :message "bye")

(irc/kill connection)
)

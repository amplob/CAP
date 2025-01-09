(ns lp
  (:require [clojure.string :as str]))

(defn eval-exp [exp symbols]
  (let [[op & args] exp]
    (case op
      :val (second exp)
      :var (get symbols (second exp))
      :add (+ (eval-exp (nth exp 1) symbols)
              (eval-exp (nth exp 2) symbols))
      :sub (- (eval-exp (nth exp 1) symbols)
              (eval-exp (nth exp 2) symbols))
      :mul (* (eval-exp (nth exp 1) symbols)
              (eval-exp (nth exp 2) symbols))
      :div (let [num (eval-exp (nth exp 1) symbols)
                 den (eval-exp (nth exp 2) symbols)]
             (if (zero? den)
               (throw (ex-info "Divisió per zero!" {}))
               (quot num den))))))  ; utilitzem integer division (quot) segons exemples

(defn process-actions [actions symbols]
  (if (empty? actions)
    symbols
    (let [action (first actions)
          rest-actions (rest actions)
          [op & args] action]
      (case op
        :ass (let [var-name (second action)
                   expr (nth action 2)
                   value (eval-exp expr symbols)
                   new-symbols (assoc symbols var-name value)]
               (process-actions rest-actions new-symbols))
        :view (do
                (try 
                  (println ">>" (eval-exp (second action) symbols))
                  (catch Exception e
                    (println ">>" (.getMessage e))))
                (process-actions rest-actions symbols))
        ;; Si trobem una acció desconeguda, simplement la saltem
        (process-actions rest-actions symbols)))))

(defn interpretar [programa]
  (process-actions programa {}))


(interpretar '((:ass "a" (:val 2))
               (:view (:div (:var "a") (:val 0)))
               (:view (:add (:var "a") (:val 1)))))
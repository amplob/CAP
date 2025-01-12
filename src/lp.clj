;Gisela Borràs Zaplana
;Marc Planas Bosch

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



;;proves
(interpretar '((:ass "a" (:val 2))
               (:view (:div (:var "a") (:val 0)))
               (:view (:add (:var "a") (:val 1)))))

(interpretar
 '((:ass "x" (:val 10))
   (:ass "y" (:mul (:var "x") (:val 2)))
   (:view (:add (:var "x") (:var "y")))))

(interpretar
 '((:ass "a" (:val 5))
   (:ass "b" (:val 3))
   (:view (:sub (:var "a") (:var "b")))
   (:view (:mul (:var "a") (:var "b")))))

(interpretar
 '((:ass "n" (:val 10))
   (:ass "d" (:val 0))
   (:view (:div (:var "n") (:var "d")))))

(interpretar
 '((:view (:var "z"))
   (:ass "z" (:val 7))
   (:view (:var "z"))))

(interpretar
 '((:ass "x" (:val 4))
   (:ass "y" (:add (:var "x") (:val 6)))
   (:ass "z" (:mul (:var "y") (:val 2)))
   (:view (:sub (:var "z") (:var "x"))) ;;20 - 4 = 16
   (:view (:div (:var "z") (:var "x"))))) ;; 20 / 4 = 5

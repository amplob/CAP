;Gisela Borràs Zaplana
;Marc Planas Bosch

(ns lp
  (:require [clojure.string :as str]))

; Funció per avaluar una expressió aritmètica donada una taula de símbols,
(defn evalua-expressio [exp taula-simbols]
  (let [[operacio & args] exp]
    (case operacio
      :val (second exp)  ; valor literal

      :var (let [[_ var] exp] (taula-simbols var))  ; accedir a la variable a la taula

      :add (let [[_ exp1 exp2] exp]
             (+ (evalua-expressio exp1 taula-simbols)
                (evalua-expressio exp2 taula-simbols)))

      :sub (let [[_ exp1 exp2] exp]
             (- (evalua-expressio exp1 taula-simbols)
                (evalua-expressio exp2 taula-simbols)))

      :mul (let [[_ exp1 exp2] exp]
             (* (evalua-expressio exp1 taula-simbols)
                (evalua-expressio exp2 taula-simbols)))

      :div (let [[_ exp1 exp2] exp
                 num (evalua-expressio exp1 taula-simbols)
                 den (evalua-expressio exp2 taula-simbols)]
             (if (zero? den)
               (throw (ex-info "Divisió per zero!" {}))
               (int (/ num den)))))))

;; Funció per processar una llista d'accions donada una taula de símbols inicial,
(defn processa-accions [accions taula-simbols]
  (if (empty? accions)
    taula-simbols
    (let [acció (first accions)
          rest-accions (rest accions)
          [operacio & args] acció]
      (case operacio
        :ass (let [[_ nom-var expr] acció
                   valor (evalua-expressio expr taula-simbols)
                   nova-taula (merge taula-simbols {nom-var valor})]
               (processa-accions rest-accions nova-taula))
        :view (let [[_ expr] acció]
                (try 
                  (println ">>" (evalua-expressio expr taula-simbols))
                  (catch Exception e
                    (println ">>" (.getMessage e))))
                (processa-accions rest-accions taula-simbols))
        (processa-accions rest-accions taula-simbols)))))

(defn interpretar [programa]
  (processa-accions programa {}))

; Joc de proves
(interpretar '((:ass "a" (:val 2))
               (:view (:div (:var "a") (:val 0))) ; divisió per zero
               (:view (:add (:var "a") (:val 1))))) ; 3

(interpretar
 '((:ass "a" (:val 5))
   (:ass "b" (:val 3))
   (:view (:sub (:var "a") (:var "b"))) ; 2
   (:view (:mul (:var "a") (:var "b"))))) ; 15

(interpretar
 '((:view (:var "z")) ; nil
   (:ass "z" (:val 7))
   (:view (:var "z")))) ; 7

(interpretar
 '((:ass "x" (:val 4))
   (:ass "y" (:add (:var "x") (:val 6)))
   (:ass "z" (:mul (:var "y") (:val 2)))
   (:view (:sub (:var "z") (:var "x"))) ; 20 - 4 = 16
   (:view (:div (:var "z") (:var "x"))))) ; 20 / 4 = 5

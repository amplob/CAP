(ns cavall)

(defn dins [posicio]
  (let [[fila col] posicio]
    (and (<= 1 fila 8) (<= 1 col 8))))


(defn moviments [posicio]
  (let [[fila col] posicio
        moviments-relatius '([-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1])]
    (filter dins (map (fn [[df dc]] [(+ fila df) (+ col dc)]) moviments-relatius))))



(defn pot-anar1 [ini fi]
    (->> ini
         moviments       
    )
)

(defn pot-anar3 [ini fi]
    (->> ini
        moviments
        (map moviments)
        (apply concat )
        (map moviments)
        (apply concat ) 
        (some #(= fi %))
    )
)


(defn moviments2 [ini]
    (->> ini
        moviments
        (map moviments)
        (apply concat )
    )
)

(defn moviments3 [ini]
    (->> ini
        moviments
        (map moviments)
        (apply concat )
        (map moviments)
        (apply concat )
    )
)


;; (defn pot-anar3 [ini fi]
;;     (let [movposs (moviments3 ini)]
;;         (filter ((fn [mov] (= fi mov)) movposs) )
;;     )
;; )

;;(defn pot-anar3 [ini fi]
;;  (let [movposs (moviments3 ini)]
;;    (some #(= fi %) movposs)))



;; Joc de proves
;; (println (dins '(4 5)))  ;; true
;; (println (dins '(0 1)))  ;; false
;; (println (dins '(4 9)))  ;; false

;; (println (moviments '(4 5))) ;; ((2 4) (2 6) (3 3) (3 7) (5 3) (5 7) (6 4) (6 6))
;; (println (moviments '(1 1))) ;; ((2 3) (3 2))

 (println (pot-anar3 '(1 1) '(4 5))) ;; true
;;  (println (pot-anar3 '(1 1) '(4 6))) ;; false

;;(println (moviments2 '(1 1))) ;; 
;(println (moviments3 '(1 1))) ;; 

;; (println (map moviments (moviments [1 1])))
;; (println (concat '(1 2 3) '(4 5)))
;; (println (apply concat (map moviments (moviments [1 1]))))
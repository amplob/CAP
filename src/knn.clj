(ns knn)


(use 'iris :reload-all)




(defn de [v w]
    (Math/sqrt
        (reduce + (map 
            (fn [vi wi] (Math/pow (- vi wi) 2)) 
            v w)
        )
    )
)

(defn dm [v w]
    (reduce + (map 
        (fn [vi wi] (Math/abs (- vi wi)) 
        ) v w)
    )
)


(defn comparaIAssigna [a b]
  (if (= a b)
    0.0
    1.0))


(defn dh [v w]
    (/
        (reduce + (map 
            (fn [vi wi] (comparaIAssigna vi wi)
            ) v w)
        )
        (count v)
    )
)

(println (de [1 0] [1 1]) )
(println (de [0 0 0] [1 1 1]) )
(println (dm [1 0] [1 1]) )
(println (dm [0 0 0] [1 1 1])  )
(println  (dh [:a :b] [:b :b]) )
(println  (dh [:a :a :a] [:b :b :b]) )



(defn knn [func_dist taula]
    (fn [ex]
        (let [distances (map (fn [t1] 
                              [(func_dist ex (:x t1)) (:y t1)]) ; Return distance and label
                            taula)   ;; lee la tabla de distancias
       nearest (apply min-key first distances)] ; Encuentra el más cercano
      (second nearest))))



;; Crear funciones KNN para iris y mushroom
(def classify-iris (knn de iris))
(def classify-mushroom (knn dh mushroom))

;; Ejecutar la clasificación
(println "Clase Iris:" (classify-iris tst-iris)) ; Devuelve la clase del vecino más cercano para iris
(println "Clase Iris:" (classify-iris tst-iris2)) ; Devuelve la clase del vecino más cercano para iris
(println "Clase Iris:" (classify-iris tst-iris3)) ; Devuelve la clase del vecino más cercano para iris

(println "Clase Mushroom:" (classify-mushroom tst-mushroom)) ; Devuelve la clase del vecino más cercano para mushroom


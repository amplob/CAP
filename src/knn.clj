;Gisela Borràs Zaplana
;Marc Planas Bosch

; hi ha dues versions del knn perquè en aquell moment encara no treballavem per parelles
; hem considerat deixar les dues (les dues funcionen)

(ns knn)
  ;(:require [p1.iris :as iris]
  ;          [p1.distancies :as dist]))
; Ara es fa tot al mateix arxiu


; Taules:
(def iris  [{:y :setosa :x [5.1 3.5 1.4 0.2]}
            {:y :setosa :x [4.9 3.0 1.4 0.2]}
            {:y :versicolor :x [6.1 2.9 4.7 1.4]}
            {:y :versicolor :x [5.6 2.9 3.6 1.3]}
            {:y :virginica :x [7.6 3.0 6.6 2.1]}
            {:y :virginica :x [4.9 2.5 4.5 1.7]}])

(def tst-iris [4.9 3.1 1.5 0.1])

(def mushroom  [{:y :poisonous :x [:convex :brown :narrow :black]}
                {:y :edible :x [:convex :yellow :broad :black]}
                {:y :edible :x [:bell :white :broad :brown]}
                {:y :poisonous :x [:convex :white :narrow :brown]}
                {:y :edible :x [:convex :yellow :broad :brown]}
                {:y :edible :x [:bell :white :broad :brown]}
                {:y :poisonous :x [:convex :white :narrow :pink]}])

(def tst-mushroom [:convex :brown :narrow :black])

; Distàncies:
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

; Proves de distàncies:
(println (de [1 0] [1 1]) )
(println (de [0 0 0] [1 1 1]) )
(println (dm [1 0] [1 1]) )
(println (dm [0 0 0] [1 1 1])  )
(println (dh [:a :b] [:b :b]) )
(println (dh [:a :a :a] [:b :b :b]) )


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
(println "Clase Mushroom:" (classify-mushroom tst-mushroom)) ; Devuelve la clase del vecino más cercano para mushroom





;funció que rep un vector de mapes "taula", un vector a evaluar "ex" i una funció per evaluar distàncies "fd"
;i retorna la clau del mapa amb el cantingut (que és un vector) que té la distància mínima a "ex" segons la
;funció per evaluar distàncies "fd"
(defn classificacio [taula ex fd]
  (let [vei-mes-proper
        (reduce
          (fn [vei-mes-proper element]
            (let [distancia (fd (:x element) ex)]
              (if (< distancia (:dist vei-mes-proper))
                {:classe (:y element) :dist distancia}
                vei-mes-proper)))
          {:classe nil :dist Double/POSITIVE_INFINITY}
          taula)]
    (println "Veí més proper trobat:" vei-mes-proper)
    (println "Classe retornada:" (:classe vei-mes-proper))
    (:classe vei-mes-proper)))


(classificacio iris tst-iris de) ;Hauria de retornar :setosa


(defn other-knn [fd taula]
  (fn [ex] (classificacio taula ex fd))
)

(def flor (other-knn de iris))
(flor tst-iris)


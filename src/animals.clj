(ns animals
  (:require [clojure.string :as str]))

;Definició d'animal utilitzant clausures
(defn animal []
  {:parlar (fn [] "grr")})

;Redefinir el mètode parlar en la subclasse gat
(defn gat []
  {:parlar (fn [] "mèu")})

;Definició de la subclasse tigre, heretant parlar d'animal
(defn tigre []
  (let [super (animal)]
    {:parlar (:parlar super)}))

;Crida n vegades a parlar d'animal
(defn parlarN [animal n]
  (println (str/join " " (repeat n ((:parlar animal))))))

;; Joc de proves:
(def a (animal))
(def g (gat))
(def t (tigre))

(parlarN a 3)  ; Esperat: grr grr grr
(parlarN g 3)  ; Esperat: mèu mèu mèu
(parlarN t 3)  ; Esperat: grr grr grr

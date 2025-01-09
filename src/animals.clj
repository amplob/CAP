(ns animales
  (:require [clojure.string :as str]))

(defprotocol Animal
  (parlar [this]))

(defrecord Gat []
  Animal
  (parlar [this] "meu"))

(defrecord Tigre []
  Animal
  (parlar [this] "grr"))

(defrecord AnimalGen []
  Animal
  (parlar [this] "grr"))

(defn parlarN [animal n]
  (println (str/join " " (repeat n (parlar animal)))))

(def animal (->AnimalGen))
(def gat (->Gat))
(def tigre (->Tigre))

(parlarN animal 3)  ; Salida: grr grr grr
(parlarN gat 3)     ; Salida: mèu mèu mèu
(parlarN tigre 3)   ; Salida: grr grr grr

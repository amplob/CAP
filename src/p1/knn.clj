(ns p1.knn
  (:require [p1.iris :as iris]
            [p1.distancies :as dist]))


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


(classificacio iris/iris iris/tst-iris dist/de) ;Hauria de retornar :setosa


(defn knn [fd taula]
  (fn [ex] (classificacio taula ex fd))
)

(def flor (knn dist/de iris/iris))
(flor iris/tst-iris)


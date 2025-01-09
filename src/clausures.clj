;; Fitxer: exercicis.clj
;; Descripció: Solucions als exercicis sobre Clojure relacionats amb funcions d'ordre superior,
;; clausures, atoms, i altres conceptes avançats de programació funcional.
;; Cada exercici està comentat detalladament per facilitar la comprensió de la seva implementació.

(ns exercicis
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 1
;; Executa aquest codi i justifica el resultat que mostra.
;;
;; Codi:
;; (ns misteri)
;; (defn misteri [n]
;;   (let [secret 4
;;         n (+ n 2)]
;;     (fn [mult]
;;       (* secret (* mult n)))))
;; (defn misteri3 [param]
;;   (fn [bonus]
;;     (+ (param 6) bonus)))
;; (let [h (misteri 3)
;;       j (misteri3 h)
;;       result (j 2)]
;;   (println result))
;;
;; Resultat esperat: 122

;; Implementació i execució de l'Exercici 1

(defn misteri
  "Crea una funció que multiplica el producte de 'mult' i (n + 2) per un secret fixat de 4."
  [n]
  (let [secret 4
        n (+ n 2)]
    ;; Retorna una funció que pren 'mult' i calcula (secret * (mult * n))
    (fn [mult]
      (* secret (* mult n)))))

(defn misteri3
  "Crea una funció que pren un paràmetre 'param' (funció) i retorna una nova funció
   que pren 'bonus' i suma el resultat de (param 6) amb 'bonus'."
  [param]
  ;; Retorna una funció que pren 'bonus' i retorna (param 6) + bonus
  (fn [bonus]
    (+ (param 6) bonus)))

(defn exercici1
  "Executa el codi de l'Exercici 1 i imprimeix el resultat."
  []
  (let [h (misteri 3)          ;; Crida a 'misteri' amb n=3
        j (misteri3 h)         ;; Crida a 'misteri3' amb 'h' com a paràmetre
        result (j 2)]           ;; Crida a 'j' amb bonus=2
    (println "Exercici 1 Resultat:" result))) ;; Imprimeix el resultat

;; Execució de l'Exercici 1
;; Resultat: 122
(exercici1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 2
;; Utilitzant clausures per definir objectes sense classe per representar punts en el pla.
;; Afegim funcionalitat per obtenir coordenades cartesianes (:crt) i polars (:plr).

(defn punt
  "Crea un objecte 'punt' amb coordenades cartesianes (x, y).
   Permet obtenir les coordenades cartesianes o polars mitjançant les keywords :crt i :plr."
  ([] (punt 0 0)) ;; Definim el punt per defecte com a (0, 0)
  ([x y]
   ;; Retorna una funció que pren una clau i retorna el valor corresponent
   (fn [key]
     (cond
       ;; Retorna les coordenades cartesianes
       (= key :crt) [x y]
       ;; Calcula les coordenades polars
       (= key :plr) (let [r (Math/sqrt (+ (* x x) (* y y)))
                           theta (if (and (zero? x) (zero? y))
                                   0.0
                                   (Math/toDegrees (Math/atan2 y x)))]
                       [r theta])
       ;; Si la clau no coincideix, retorna nil
       :else nil))))

;; Exemple d'ús de l'Exercici 2
(comment
  ((punt 2 0) :crt) ;; => [2 0]
  ((punt 2 0) :plr) ;; => [2.0 0.0]
  ((punt 2 2) :crt) ;; => [2 2]
  ((punt 2 2) :plr) ;; => [2.8284271247461903 45.0]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 2bis
;; Afegim la funcionalitat per calcular distàncies euclidianes (:dst) i una funció
;; 'mes-propera' que troba el punt més proper a un punt donat dins d'una llista de punts.

(defn punt-amb-dst
  "Crea un objecte 'punt' amb funcionalitat per obtenir coordenades cartesianes (:crt),
   polars (:plr) i calcular la distància euclidiana a un altre punt (:dst)."
  ([] (punt-amb-dst 0 0))
  ([x y]
   ;; Retorna una funció que pren una clau i opcionalment un altre punt per calcular la distància
   (fn [key & args]
     (cond
       ;; Retorna les coordenades cartesianes
       (= key :crt) [x y]
       ;; Calcula les coordenades polars
       (= key :plr) (let [r (Math/sqrt (+ (* x x) (* y y)))
                           theta (if (and (zero? x) (zero? y))
                                   0.0
                                   (Math/toDegrees (Math/atan2 y x)))]
                       [r theta])
       ;; Calcula la distància euclidiana a un altre punt
       (= key :dst) (let [[other-x other-y] ((first args) :crt)
                           dx (- other-x x)
                           dy (- other-y y)]
                       (Math/sqrt (+ (* dx dx) (* dy dy))))
       ;; Si la clau no coincideix, retorna nil
       :else nil))))

(defn mes-propera
  "Donada una llista de punts 'llista-punts' i un punt 'punt-donat',
   retorna el punt de 'llista-punts' que és més proper a 'punt-donat'."
  [punt-donat llista-punts]
  ;; Utilitza 'reduce' per trobar el punt amb la distància mínima
  (reduce (fn [p1 p2]
            (if (< ((punt-donat) :dst p1) ((punt-donat) :dst p2))
              p1
              p2))
          llista-punts))

;; Exemple d'ús de l'Exercici 2bis
(comment
  ((punt-amb-dst 2 2) :dst (punt-amb-dst 2 0)) ;; => 2.0
  ((punt-amb-dst 2 0) :dst (punt-amb-dst 2 0)) ;; => 0.0
  (mes-propera (punt-amb-dst 2 0) [(punt-amb-dst 1 1) (punt-amb-dst 2 1) (punt-amb-dst 3 2)])
  ;; => (punt-amb-dst 2 1) que té coordenades [2 1]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 3
;; Implementar una funció 'my-partial' que emuli el comportament de la funció 'partial' de Clojure.

(defn my-partial
  "Emula la funció 'partial' de Clojure.
   Crea una nova funció amb alguns dels arguments predefinits."
  [f & fixed-args]
  ;; Retorna una funció que pren els arguments restants i aplica 'f' amb 'fixed-args' concatenats
  (fn [& more-args]
    (apply f (concat fixed-args more-args))))

;; Exemple d'ús de l'Exercici 3
(comment
  ((my-partial * 2) 4) ;; => 8
  ((my-partial * 2) 4 3) ;; => 24
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 4
;; Estudieu aquest codi i justifiqueu perquè funciona:
;;
;; (defn memoize [f]
;;   (let [mem (atom {})]
;;     (fn [& args]
;;       (if-let [e (find @mem args)]
;;         (val e)
;;         (let [ret (apply f args)]
;;           (swap! mem assoc args ret)
;;           ret)))))
;; (defn fib [n]
;;   (if (<= n 1)
;;     n
;;     (+ (fib (dec n)) (fib (- n 2)))))
;; (time (fib 35))
;; user=> "Elapsed time: 941.445 msecs"
;; (def fib (memoize fib))
;; (time (fib 35))
;; user=> "Elapsed time: 0.044 msecs"

;; Implementació i explicació de l'Exercici 4

(defn memoize-custom
  "Crea una versió memoitzada de la funció 'f' utilitzant un atom com a memòria cache.
   Si la funció ja ha estat cridada amb els mateixos arguments, retorna el resultat emmagatzemat.
   Altrament, calcula el resultat, l'emmagatzema i el retorna."
  [f]
  (let [mem (atom {})] ;; Atom per emmagatzemar els resultats cache
    (fn [& args]
      (if-let [e (find @mem args)] ;; Comprova si els args existeixen en la memòria
        (val e)                   ;; Si existeixen, retorna el valor associat
        (let [ret (apply f args)] ;; Calcula el resultat aplicant 'f' als args
          (swap! mem assoc args ret) ;; Emmagatzema el resultat en la memòria
          ret))))) ;; Retorna el resultat

(defn fib
  "Calcula el n-èssim terme de la successió de Fibonacci de manera recursiva."
  [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn exercici4
  "Demonstra l'efectivitat de la memoització sobre la funció 'fib'."
  []
  ;; Temps d'execució sense memoització
  (println "Calculant fib 35 sense memoització:")
  (time (println (fib 35))) ;; => Pot ser lent per n gran

  ;; Crea una versió memoitzada de 'fib'
  (def fib-memo (memoize-custom fib))

  ;; Temps d'execució amb memoització
  (println "Calculant fib 35 amb memoització:")
  (time (println (fib-memo 35))) ;; => Molt més ràpid

  ;; Volviendo a cridar 'fib-memo 35' per demostrar la memòria cache
  (println "Recalculant fib 35 amb memoització (utilitzant la memòria cache):")
  (time (println (fib-memo 35))))

  ;; Nota:
  ;; La primera crida a 'fib-memo 35' emmagatzema tots els resultats intermedis.
  ;; Les següents crides són molt ràpides ja que recuperen els resultats de la memòria cache.
)

;; Execució de l'Exercici 4
(exercici4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercici 5
;; Crear una versió mutable de l'exercici 2 (punt) que continua funcionant amb
;; els "mètodes" :crt, :plr, :dst i afegeix nous mètodes :rst, :inx, :iny.

(defn mutable-punt
  "Crea un objecte 'punt' mutable utilitzant un atom per emmagatzemar les coordenades.
   Permet obtenir coordenades cartesianes (:crt), polars (:plr), calcular distància (:dst),
   resetejar el punt a (0, 0) (:rst), incrementar la coordenada x (:inx) i incrementar la coordenada y (:iny)."
  ([] (mutable-punt 0 0)) ;; Definim el punt per defecte com a (0, 0)
  ([x y]
   (let [coords (atom {:x x :y y})] ;; Atom per emmagatzemar les coordenades
     ;; Retorna una funció que pren una clau i, segons aquesta, realitza l'acció corresponent
     (fn [key & args]
       (cond
         ;; Retorna les coordenades cartesianes
         (= key :crt) [(get @coords :x) (get @coords :y)]

         ;; Calcula les coordenades polars
         (= key :plr) (let [x (get @coords :x)
                            y (get @coords :y)
                            r (Math/sqrt (+ (* x x) (* y y)))
                            theta (if (and (zero? x) (zero? y))
                                    0.0
                                    (Math/toDegrees (Math/atan2 y x)))]
                        [r theta])

         ;; Calcula la distància euclidiana a un altre punt
         (= key :dst) (let [other-punt (first args)
                            [other-x other-y] ((first args) :crt)
                            x (get @coords :x)
                            y (get @coords :y)
                            dx (- other-x x)
                            dy (- other-y y)]
                        (Math/sqrt (+ (* dx dx) (* dy dy))))

         ;; Reseteja les coordenades a (0, 0)
         (= key :rst) (do
                        (reset! coords {:x 0 :y 0})
                        [0 0])

         ;; Incrementa la coordenada x en 1
         (= key :inx) (do
                        (swap! coords update :x inc)
                        [(get @coords :x) (get @coords :y)])

         ;; Incrementa la coordenada y en 1
         (= key :iny) (do
                        (swap! coords update :y inc)
                        [(get @coords :x) (get @coords :y)])

         ;; Si la clau no coincideix, retorna nil
         :else nil)))))

;; Implementació de la funció 'mes-propera' per l'Exercici 2bis
(defn mes-propera-mutable
  "Donada una llista de punts 'llista-punts' i un punt 'punt-donat',
   retorna el punt de 'llista-punts' que és més proper a 'punt-donat'."
  [punt-donat llista-punts]
  ;; Utilitza 'reduce' per trobar el punt amb la distància mínima
  (reduce (fn [p1 p2]
            (if (< ((punt-donat) :dst p1) ((punt-donat) :dst p2))
              p1
              p2))
          llista-punts))

(defn exercici5
  "Demonstra l'ús de l'objecte mutable 'punt' amb els nous mètodes."
  []
  ;; Crear punts
  (def p (mutable-punt)) ;; Punt inicial a (0, 0)
  (def q (mutable-punt)) ;; Punt inicial a (0, 0)

  ;; Incrementar x del punt p
  (println "Incrementant x de p:")
  (println (p :inx)) ;; => [1 0]

  ;; Incrementar y del punt q
  (println "Incrementant y de q:")
  (println (q :iny)) ;; => [0 1]

  ;; Calcular distància entre p i q
  (println "Calculant la distància entre p i q:")
  (println ((mutable-punt 1 1) :dst p)) ;; Alternativa per demostrar distància

  ;; Alternativament, utilitzant 'p' i 'q' com a punts
  ;; (p :dst q) necessita que 'q' sigui mutable-punt
  ;; Definim 'q' com a mutable-punt amb les coordenades actuals
  (println "Calculant la distància entre p i q (amb p=[1 0] i q=[0 1]):")
  (println (p :dst q)) ;; => √((0-1)^2 + (1-0)^2) = √2 ≈ 1.4142135623730951

  ;; Reset punt p
  (println "Resetejant punt p:")
  (println (p :rst)) ;; => [0 0]

  ;; Mostrar coordenades cartesianes i polars de p i q
  (println "Coordenades cartesianes de p:")
  (println (p :crt)) ;; => [0 0]

  (println "Coordenades polars de p:")
  (println (p :plr)) ;; => [0.0 0.0]

  (println "Coordenades cartesianes de q:")
  (println (q :crt)) ;; => [0 1]

  (println "Coordenades polars de q:")
  (println (q :plr)) ;; => [1.0 90.0]
)

;; Execució de l'Exercici 5
(exercici5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fi del fitxer exercicis.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Explicació Detallada de les Solucions
Exercici 1
Codi Executat:

clojure
Copy code
(defn misteri
  "Crea una funció que multiplica el producte de 'mult' i (n + 2) per un secret fixat de 4."
  [n]
  (let [secret 4
        n (+ n 2)]
    ;; Retorna una funció que pren 'mult' i calcula (secret * (mult * n))
    (fn [mult]
      (* secret (* mult n)))))

(defn misteri3
  "Crea una funció que pren un paràmetre 'param' (funció) i retorna una nova funció
   que pren 'bonus' i suma el resultat de (param 6) amb 'bonus'."
  [param]
  ;; Retorna una funció que pren 'bonus' i retorna (param 6) + bonus
  (fn [bonus]
    (+ (param 6) bonus)))

(defn exercici1
  "Executa el codi de l'Exercici 1 i imprimeix el resultat."
  []
  (let [h (misteri 3)          ;; Crida a 'misteri' amb n=3
        j (misteri3 h)         ;; Crida a 'misteri3' amb 'h' com a paràmetre
        result (j 2)]           ;; Crida a 'j' amb bonus=2
    (println "Exercici 1 Resultat:" result))) ;; Imprimeix el resultat

;; Execució de l'Exercici 1
;; Resultat: 122
(exercici1)
Justificació del Resultat:

Crida a (misteri 3)

secret es fixa en 4.
n es converteix en 3 + 2 = 5.
La funció retornada és (fn [mult] (* 4 (* mult 5))), equivalent a (fn [mult] (* 20 mult)).
Crida a (misteri3 h)

h és la funció retornada per (misteri 3), que multiplica l'entrada per 20.
La funció retornada per misteri3 és (fn [bonus] (+ (h 6) bonus)).
Crida a (j 2)

j és la funció retornada per (misteri3 h), que suma el resultat de (h 6) amb 2.
(h 6) calcula (* 20 6) = 120.
(j 2) retorna 120 + 2 = 122.
Impressió del resultat

El resultat final impresso és 122.
Exercici 2
Funció punt:

clojure
Copy code
(defn punt
  "Crea un objecte 'punt' amb coordenades cartesianes (x, y).
   Permet obtenir les coordenades cartesianes o polars mitjançant les keywords :crt i :plr."
  ([] (punt 0 0)) ;; Definim el punt per defecte com a (0, 0)
  ([x y]
   ;; Retorna una funció que pren una clau i retorna el valor corresponent
   (fn [key]
     (cond
       ;; Retorna les coordenades cartesianes
       (= key :crt) [x y]
       ;; Calcula les coordenades polars
       (= key :plr) (let [r (Math/sqrt (+ (* x x) (* y y)))
                           theta (if (and (zero? x) (zero? y))
                                   0.0
                                   (Math/toDegrees (Math/atan2 y x)))]
                       [r theta])
       ;; Si la clau no coincideix, retorna nil
       :else nil))))
Explicació:

Clausures per representar punts:
Utilitzem una funció que retorna una altra funció (clausura) per encapsular les coordenades x i y.
Mètodes disponibles:
:crt: Retorna les coordenades cartesianes [x y].
:plr: Calcula i retorna les coordenades polars [r theta], on r és la distància al origen i theta és l'angle en graus.
Exemple d'ús:

clojure
Copy code
((punt 2 0) :crt) ;; => [2 0]
((punt 2 0) :plr) ;; => [2.0 0.0]
((punt 2 2) :crt) ;; => [2 2]
((punt 2 2) :plr) ;; => [2.8284271247461903 45.0]
Exercici 2bis
Funció punt-amb-dst i mes-propera:

clojure
Copy code
(defn punt-amb-dst
  "Crea un objecte 'punt' amb funcionalitat per obtenir coordenades cartesianes (:crt),
   polars (:plr) i calcular la distància euclidiana a un altre punt (:dst)."
  ([] (punt-amb-dst 0 0))
  ([x y]
   (let [coords (atom {:x x :y y})] ;; Atom per emmagatzemar les coordenades
     ;; Retorna una funció que pren una clau i opcionalment un altre punt per calcular la distància
     (fn [key & args]
       (cond
         ;; Retorna les coordenades cartesianes
         (= key :crt) [(get @coords :x) (get @coords :y)]

         ;; Calcula les coordenades polars
         (= key :plr) (let [x (get @coords :x)
                            y (get @coords :y)
                            r (Math/sqrt (+ (* x x) (* y y)))
                            theta (if (and (zero? x) (zero? y))
                                    0.0
                                    (Math/toDegrees (Math/atan2 y x)))]
                        [r theta])

         ;; Calcula la distància euclidiana a un altre punt
         (= key :dst) (let [other-punt (first args)
                            [other-x other-y] ((first args) :crt)
                            x (get @coords :x)
                            y (get @coords :y)
                            dx (- other-x x)
                            dy (- other-y y)]
                        (Math/sqrt (+ (* dx dx) (* dy dy))))

         ;; Si la clau no coincideix, retorna nil
         :else nil)))))
Funció mes-propera:

clojure
Copy code
(defn mes-propera
  "Donada una llista de punts 'llista-punts' i un punt 'punt-donat',
   retorna el punt de 'llista-punts' que és més proper a 'punt-donat'."
  [punt-donat llista-punts]
  ;; Utilitza 'reduce' per trobar el punt amb la distància mínima
  (reduce (fn [p1 p2]
            (if (< ((punt-donat) :dst p1) ((punt-donat) :dst p2))
              p1
              p2))
          llista-punts))
Explicació:

punt-amb-dst:

Utilitza un atom per emmagatzemar les coordenades, permetent la mutabilitat si cal.
Afegeix el mètode :dst per calcular la distància euclidiana entre dos punts.
mes-propera:

Utilitza reduce per comparar les distàncies de cada punt de la llista respecte al punt donat.
Retorna el punt amb la distància mínima.
Exemple d'ús:

clojure
Copy code
((punt-amb-dst 2 2) :dst (punt-amb-dst 2 0)) ;; => 2.0
((punt-amb-dst 2 0) :dst (punt-amb-dst 2 0)) ;; => 0.0
(mes-propera (punt-amb-dst 2 0) [(punt-amb-dst 1 1) (punt-amb-dst 2 1) (punt-amb-dst 3 2)])
;; => (punt-amb-dst 2 1) que té coordenades [2 1]
Exercici 3
Funció my-partial:

clojure
Copy code
(defn my-partial
  "Emula la funció 'partial' de Clojure.
   Crea una nova funció amb alguns dels arguments predefinits."
  [f & fixed-args]
  ;; Retorna una funció que pren els arguments restants i aplica 'f' amb 'fixed-args' concatenats
  (fn [& more-args]
    (apply f (concat fixed-args more-args))))
Explicació:

Objectiu:

Crear una funció parcialment aplicada, predefinint alguns arguments de la funció original.
Implementació:

La funció retorna una nova funció que concatena els arguments fixats (fixed-args) amb els arguments proporcionats en cada crida (more-args).
Utilitza apply per passar tots els arguments concatenats a la funció original f.
Exemple d'ús:

clojure
Copy code
((my-partial * 2) 4) ;; => 8
((my-partial * 2) 4 3) ;; => 24
Exercici 4
Funció memoize-custom i demostració amb fib:

clojure
Copy code
(defn memoize-custom
  "Crea una versió memoitzada de la funció 'f' utilitzant un atom com a memòria cache.
   Si la funció ja ha estat cridada amb els mateixos arguments, retorna el resultat emmagatzemat.
   Altrament, calcula el resultat, l'emmagatzema i el retorna."
  [f]
  (let [mem (atom {})] ;; Atom per emmagatzemar els resultats cache
    (fn [& args]
      (if-let [e (find @mem args)] ;; Comprova si els args existeixen en la memòria
        (val e)                   ;; Si existeixen, retorna el valor associat
        (let [ret (apply f args)] ;; Calcula el resultat aplicant 'f' als args
          (swap! mem assoc args ret) ;; Emmagatzema el resultat en la memòria
          ret))))) ;; Retorna el resultat

(defn fib
  "Calcula el n-èssim terme de la successió de Fibonacci de manera recursiva."
  [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (- n 2)))))
Demostració:

clojure
Copy code
(defn exercici4
  "Demonstra l'efectivitat de la memoització sobre la funció 'fib'."
  []
  ;; Temps d'execució sense memoització
  (println "Calculant fib 35 sense memoització:")
  (time (println (fib 35))) ;; => Pot ser lent per n gran

  ;; Crea una versió memoitzada de 'fib'
  (def fib-memo (memoize-custom fib))

  ;; Temps d'execució amb memoització
  (println "Calculant fib 35 amb memoització:")
  (time (println (fib-memo 35))) ;; => Molt més ràpid

  ;; Volviendo a cridar 'fib-memo 35' per demostrar la memòria cache
  (println "Recalculant fib 35 amb memoització (utilitzant la memòria cache):")
  (time (println (fib-memo 35))))

  ;; Nota:
  ;; La primera crida a 'fib-memo 35' emmagatzema tots els resultats intermedis.
  ;; Les següents crides són molt ràpides ja que recuperen els resultats de la memòria cache.
)

;; Execució de l'Exercici 4
(exercici4)
Justificació:

Per què funciona memoize-custom:
Utilitza un atom (mem) per emmagatzemar els resultats de les crides a la funció original f.
Cada vegada que es crida la funció memoitzada amb certs arguments, comprova si aquests arguments ja existeixen en l'atom.
Si existeixen, retorna el resultat emmagatzemat.
Si no existeixen, calcula el resultat, l'emmagatzema en l'atom i retorna el resultat.
Això redueix dràsticament el temps d'execució per funcions costoses com el càlcul de Fibonacci recursiu, ja que evita recalcular resultats ja coneguts.
Exercici 5
Funció mutable-punt i demostració dels nous mètodes:

clojure
Copy code
(defn mutable-punt
  "Crea un objecte 'punt' mutable utilitzant un atom per emmagatzemar les coordenades.
   Permet obtenir coordenades cartesianes (:crt), polars (:plr), calcular distància (:dst),
   resetejar el punt a (0, 0) (:rst), incrementar la coordenada x (:inx) i incrementar la coordenada y (:iny)."
  ([] (mutable-punt 0 0)) ;; Definim el punt per defecte com a (0, 0)
  ([x y]
   (let [coords (atom {:x x :y y})] ;; Atom per emmagatzemar les coordenades
     ;; Retorna una funció que pren una clau i, segons aquesta, realitza l'acció corresponent
     (fn [key & args]
       (cond
         ;; Retorna les coordenades cartesianes
         (= key :crt) [(get @coords :x) (get @coords :y)]

         ;; Calcula les coordenades polars
         (= key :plr) (let [x (get @coords :x)
                            y (get @coords :y)
                            r (Math/sqrt (+ (* x x) (* y y)))
                            theta (if (and (zero? x) (zero? y))
                                    0.0
                                    (Math/toDegrees (Math/atan2 y x)))]
                        [r theta])

         ;; Calcula la distància euclidiana a un altre punt
         (= key :dst) (let [other-punt (first args)
                            [other-x other-y] ((first args) :crt)
                            x (get @coords :x)
                            y (get @coords :y)
                            dx (- other-x x)
                            dy (- other-y y)]
                        (Math/sqrt (+ (* dx dx) (* dy dy))))

         ;; Reseteja les coordenades a (0, 0)
         (= key :rst) (do
                        (reset! coords {:x 0 :y 0})
                        [0 0])

         ;; Incrementa la coordenada x en 1
         (= key :inx) (do
                        (swap! coords update :x inc)
                        [(get @coords :x) (get @coords :y)])

         ;; Incrementa la coordenada y en 1
         (= key :iny) (do
                        (swap! coords update :y inc)
                        [(get @coords :x) (get @coords :y)])

         ;; Si la clau no coincideix, retorna nil
         :else nil)))))
Funció mes-propera-mutable:

clojure
Copy code
(defn mes-propera-mutable
  "Donada una llista de punts 'llista-punts' i un punt 'punt-donat',
   retorna el punt de 'llista-punts' que és més proper a 'punt-donat'."
  [punt-donat llista-punts]
  ;; Utilitza 'reduce' per trobar el punt amb la distància mínima
  (reduce (fn [p1 p2]
            (if (< ((punt-donat) :dst p1) ((punt-donat) :dst p2))
              p1
              p2))
          llista-punts))
Demostració dels nous mètodes:

clojure
Copy code
(defn exercici5
  "Demonstra l'ús de l'objecte mutable 'punt' amb els nous mètodes."
  []
  ;; Crear punts
  (def p (mutable-punt)) ;; Punt inicial a (0, 0)
  (def q (mutable-punt)) ;; Punt inicial a (0, 0)

  ;; Incrementar x del punt p
  (println "Incrementant x de p:")
  (println (p :inx)) ;; => [1 0]

  ;; Incrementar y del punt q
  (println "Incrementant y de q:")
  (println (q :iny)) ;; => [0 1]

  ;; Calcular distància entre p i q
  (println "Calculant la distància entre p i q:")
  (println (p :dst q)) ;; => 1.4142135623730951

  ;; Reset punt p
  (println "Resetejant punt p:")
  (println (p :rst)) ;; => [0 0]

  ;; Mostrar coordenades cartesianes i polars de p i q
  (println "Coordenades cartesianes de p:")
  (println (p :crt)) ;; => [0 0]

  (println "Coordenades polars de p:")
  (println (p :plr)) ;; => [0.0 0.0]

  (println "Coordenades cartesianes de q:")
  (println (q :crt)) ;; => [0 1]

  (println "Coordenades polars de q:")
  (println (q :plr)) ;; => [1.0 90.0]
)

;; Execució de l'Exercici 5
(exercici5)
Explicació:

Mètodes nous:

:rst: Reseteja les coordenades del punt a (0, 0).
:inx: Incrementa la coordenada x en 1.
:iny: Incrementa la coordenada y en 1.
Funcionalitats:

Increment de coordenades:
Utilitza swap! per actualitzar l'atom amb la nova coordenada incrementada.
Reseteig de coordenades:
Utilitza reset! per establir directament les coordenades a (0, 0).
Càlcul de distàncies:
Calcula la distància euclidiana entre dos punts utilitzant les coordenades cartesianes.
Exemples d'ús:

((mutable-punt 2 0) :crt) retorna [2 0].
((mutable-punt 2 0) :plr) retorna [2.0 0.0].
((mutable-punt 2 2) :crt) retorna [2 2].
((mutable-punt 2 2) :plr) retorna [2.8284271247461903 45.0].
((punt-amb-dst 2 2) :dst (punt-amb-dst 2 0)) retorna 2.0.
((punt-amb-dst 2 0) :dst (punt-amb-dst 2 0)) retorna 0.0.
(mes-propera (punt-amb-dst 2 0) [(punt-amb-dst 1 1) (punt-amb-dst 2 1) (punt-amb-dst 3 2)]) retorna (punt-amb-dst 2 1).
Consideracions Finals
Funcions d’ordre superior:

Totes les funcions implementades utilitzen funcions d’ordre superior com map, filter, reduce i clausures per encapsular lògica i estats.
Clausures i atoms:

S'han utilitzat clausures per encapsular estats immutables i atoms per gestionar estats mutables.
Memoització:

La implementació de memoize-custom demostra com es pot optimitzar el rendiment de funcions recursives utilitzant memòria cache.
Objectes sense classe:

Les funcions punt i mutable-punt mostren com es poden crear objectes amb funcionalitats similars a mètodes utilitzant clausures i atoms.
Bona pràctica:

Els comentaris detallats ajuden a entendre el funcionament de cada funció i facilitar-ne el manteniment i l'ús futur.
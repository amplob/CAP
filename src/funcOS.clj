;; Fitxer: funcions_ordres_superiors.clj
;; Descripció: Aquest fitxer conté diverses funcions en Clojure que utilitzen funcions d’ordre superior
;; per resoldre diferents problemes sense utilitzar recursivitat. Cada funció està comentada detalladament
;; per explicar la seva implementació i inclou exemples d’entrada i sortida.

(ns funcions-ordre-superiors
  (:require [clojure.string :as str]))

;; 1. Funció `eql`
;; Aquesta funció indica si dues llistes d’enters són iguals. Es considera que dues llistes són iguals
;; si tenen la mateixa longitud i tots els elements corresponents són iguals.

(defn eql
  "Indica si dues llistes d’enters `list1` i `list2` són iguals.
  Dos llistes són iguals si tenen la mateixa longitud i tots els elements corresponents són iguals."
  [list1 list2]
  ;; Primer, comprovem si les dues llistes tenen la mateixa longitud.
  (and (= (count list1) (count list2))
       ;; Si tenen la mateixa longitud, avaluem si tots els elements corresponents són iguals.
       ;; Utilitzem `every?` amb `map` per comparar els elements.
       (every? true?
               (map = list1 list2))))

;; Exemple d’ús:
;; (eql '(1 2 3) '(1 2 3)) => true
;; (eql '(1 2 3) '(3 2 1)) => false
;; (eql '(1 2 3) '(1 2 3 4)) => false

;; 2. Funció `prod-of-evens`
;; Aquesta funció multiplica tots els nombres parells d’una llista d’enters. Si no hi ha nombres parells,
;; retorna 1 (element neutre de la multiplicació).

(defn prod-of-evens
  "Multiplica tots els nombres parells de la llista `coll`.
  Si no hi ha nombres parells, retorna 1."
  [coll]
  ;; Utilitzem `filter` per seleccionar només els nombres parells.
  (let [evens (filter even? coll)]
    ;; Utilitzem `reduce` amb la funció de multiplicació `*` per multiplicar tots els elements.
    ;; Si la llista de nombres parells és buida, `reduce` retorna 1 per defecte.
    (reduce * 1 evens)))

;; Exemple d’ús:
;; (prod-of-evens '(2 10 5)) => 20

;; 3. Funció `my-reverse`
;; Aquesta funció inverteix els elements d’una llista d’enters sense utilitzar recursivitat.
;; Utilitza la funció `reduce` combinada amb `conj` per construir la llista invertida.

(defn my-reverse
  "Inverteix els elements de la llista `coll`."
  [coll]
  ;; Utilitzem `reduce` per acumular els elements en ordre invers.
  ;; La funció `conj` afegeix elements al davant d'una llista, invertint així l'ordre.
  (reduce (fn [acc x]
            (conj acc x))
          '()  ;; Inicialitzem l'acumulador com una llista buida.
          coll))

;; Exemple d’ús:
;; (my-reverse '(1 2 3 4)) => (4 3 2 1)

;; 4. Funció `scalar-product`
;; Aquesta funció calcula el producte escalar de dues llistes de reals de la mateixa mida.
;; El producte escalar és la suma dels productes dels elements corresponents de les dues llistes.

(defn scalar-product
  "Calcula el producte escalar de dues llistes de reals `list1` i `list2` de la mateixa mida."
  [list1 list2]
  ;; Utilitzem `map` amb la funció de multiplicació `*` per obtenir els productes dels elements corresponents.
  ;; Després, sumem tots els productes utilitzant `reduce` amb `+`.
  (reduce +
          (map * list1 list2)))

;; Exemple d’ús:
;; (scalar-product '(2.0 1.0) '(3.0 2.0)) => 8.0

;; 5. Funció `count-in`
;; Aquesta funció, donada una llista de llistes d’elements `llist` i un element `x`,
;; retorna una llista que indica quantes vegades apareix `x` en cada subllista de `llist`.

(defn count-in
  "Donada una llista de llistes `llist` i un element `x`, retorna una llista
   que indica quantes vegades apareix `x` en cada subllista."
  [llist x]
  ;; Utilitzem `map` per aplicar una funció a cada subllista de `llist`.
  ;; La funció compta les ocurrències de `x` en cada subllista utilitzant `filter` i `count`.
  (map (fn [sublist]
         (count (filter #(= % x) sublist)))
       llist))

;; Exemple d’ús:
;; (count-in '((3 2 3) (3) () (2 2)) 3) => (2 1 0 0)

;; 6. Funció `first-word`
;; Aquesta funció, donat un string amb espais i caràcters alfabètics, retorna la primera paraula.
;; Una paraula es defineix com una seqüència de caràcters alfabètics separada per espais.

(defn first-word
  "Donat un string `s` amb espais i caràcters alfabètics, retorna la primera paraula."
  [s]
  ;; Utilitzem `clojure.string/trim` per eliminar espais al principi i al final del string.
  ;; Després, fem servir `clojure.string/split` per dividir el string en paraules basant-nos en els espais.
  ;; Finalment, prenem la primera paraula amb `first`.
  (first (str/split (str/trim s) #"\s+")))

;; Exemple d’ús:
;; (first-word " Volem pa amb oli ") => "Volem"

;; Fi del fitxer funcions_ordres_superiors.clj

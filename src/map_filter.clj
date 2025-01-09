;; Fitxer: solucions.clj
;; Descripció: Aquest fitxer conté diverses funcions en Clojure que emulen les funcions
;; estàndard `map`, `filter`, `map` amb dues llistes (`zip-with`), `thingify` i `factors`.
;; Cada funció està comentada detalladament per explicar la seva implementació.
;; Inclou exemples d'entrada i sortida per a cada funció.

;; 1. Funció `my-map`
;; Aquesta funció emula el comportament de `map` utilitzant la macro `for` de Clojure.
;; Pren una funció `f` i una llista `coll`, i aplica `f` a cada element de `coll`.

(defn my-map
  "Emula la funció `map` aplicant la funció `f` a cada element de la col·lecció `coll`."
  [f coll]
  ;; Utilitzem la macro `for` per iterar sobre cada element de `coll`.
  ;; La variable `x` representa cada element individual de `coll`.
  (for [x coll]
    ;; Apliquem la funció `f` a cada element `x` i construïm una nova llista amb els resultats.
    (f x)))

;; Exemple d'ús:
;; (my-map #(* % 2) (range 1 5)) => (2 4 6 8)

;; 2. Funció `my-filter`
;; Aquesta funció emula el comportament de `filter` utilitzant la macro `for`.
;; Pren una funció predicat `pred` i una llista `coll`, retornant només els elements que compleixen el predicat.

(defn my-filter
  "Emula la funció `filter` seleccionant els elements de `coll` que satisfan el predicat `pred`."
  [pred coll]
  ;; Utilitzem la macro `for` amb una condició `:when` per filtrar els elements.
  (for [x coll
        ;; La clàusula `:when` assegura que només els elements que compleixen `pred` siguin inclosos.
        :when (pred x)]
    ;; Incloem l'element `x` a la nova llista només si compleix el predicat.
    x))

;; Exemple d'ús:
;; (my-filter odd? (range 1 5)) => (1 3)

;; 3. Funció `my-zip-with`
;; Aquesta funció emula `map` però amb dues col·leccions, aplicant una funció `f` a cada parell d'elements corresponents.
;; Si les llistes tenen longituds diferents, es processaran fins a la longitud de la llista més curta.

(defn my-zip-with
  "Emula `map` amb dues col·leccions, aplicant la funció `f` a cada parell d'elements."
  [f coll1 coll2]
  ;; Calculem la longitud mínima de les dues col·leccions per evitar errors si tenen llargues diferents.
  (let [len (min (count coll1) (count coll2))]
    ;; Iterem sobre els índexs fins a la longitud mínima de les dues col·leccions.
    (for [i (range len)]
      ;; Apliquem `f` als elements corresponents de `coll1` i `coll2`.
      (f (nth coll1 i) (nth coll2 i)))))

;; Exemple d'ús:
;; (my-zip-with * (range 1 4) (range 1 4)) => (1 4 9)

;; 4. Funció `thingify`
;; Aquesta funció pren dues llistes d'enters i retorna una llista de parells [x y]
;; on y divideix a x. Això implica generar totes les combinacions possibles de les dues llistes
;; i seleccionar només aquelles on y divideix a x.

(defn thingify
  "Donades dues llistes d'enters `list1` i `list2`, retorna una llista
   de parells [x y] on y divideix a x."
  [list1 list2]
  ;; Utilitzem una comprensió de llistes doble per generar totes les combinacions possibles.
  (for [x list1
        y list2
        ;; La clàusula `:when` assegura que només incloem el parell [x y] si y divideix a x.
        :when (and (not= y 0) ;; Evitem la divisió per zero.
                   (= (mod x y) 0))] ;; Comprovem que y divideix x sense residu.
    ;; Incloem el parell [x y] només si y divideix a x.
    [x y]))

;; Exemple d'ús:
;; (thingify (range 1 6) (range 1 3)) => ([1 1] [2 1] [2 2] [3 1] [4 1] [4 2] [5 1])

;; 5. Funció `factors`
;; Aquesta funció genera una llista ordenada de factors d'un nombre natural donat (no necessàriament primers).

(defn factors
  "Genera una llista ordenada de factors del nombre natural `n`."
  [n]
  ;; Validem que `n` sigui un nombre natural no nul.
  (if (and (integer? n) (> n 0))
    ;; Utilitzem `for` per iterar des de 1 fins a `n` (inclusiu) i seleccionem els divisors.
    (sort
      (for [x (range 1 (inc n))
            ;; La condició assegura que `x` divideix `n` sense residu.
            :when (= (mod n x) 0)]
        x))
    ;; Si `n` no és vàlid, retornem una llista buida.
    '()))

;; Exemple d'ús:
;; (factors 24) => (1 2 3 4 6 8 12 24)

;; Fi del fitxer solucions.clj

(ns firstclass1)



;Escriu una funció que, donada una llista d'enters, filtri els nombres parells, 
;multipliqui cada nombre per 2 i després sumi tots els elements.

(defn mult2 [a]
    (* 2 a)
)
(defn ParMult2Sum [l]
    (reduce + (map mult2 (filter even? l)))
)
(println (ParMult2Sum '(1 2 3 4 5))) ; 

(defn process-list [nums]
    (->> nums
       (filter even?)               ; Filtra els nombres parells
       (map #(* % 2))               ; Multiplica cada nombre per 2
       (reduce +)                   ; Suma tots els elements
    )
)                 

;Escriu una funció que, donat un map de noms i edats, 
;retorni un vector dels noms de les persones majors de 18 anys, ordenades alfabèticament.

(defn nomMajor18 [ne]
    ()
)

(defn majors18Ord [ne]
    (map nomMajor18 ne)
)
(println (majors18Ord {"Alice" 20, "Bob" 17, "Charlie" 22, "David" 15})) 

(defn adults-sorted [people]
  (->> people
       (filter (fn [[_ age]] (> age 18))) ; Filtra les persones majors de 18 anys
       (map first)                        ; Extreu només els noms
       (sort)))                           ; Ordena els noms alfabèticament




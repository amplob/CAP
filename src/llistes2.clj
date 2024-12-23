(ns llistes2)

(defn build-palindrome [l]
    (loop [ tmp l
            lst l]
        (if (empty? l)
            lst
            (recur (rest tmp) (conj lst (first tmp)))
        )
    )
)

;treu l'element e de la llista l
;retorna una llista sense l'element e
(defn remove-element [  l
                        e]
    (let [ret '()]
        (loop [ tmp l
                ret ret]
            (if (empty? tmp)
                ret
                (if (= e (first tmp))
                    (recur (rest tmp) ret)
                    (recur (rest tmp) (conj ret (first tmp)))
                ) 
            )
        )
    )
)

;donada una llista d’enters x i una llista d’enters y,
;retorna la llista x havent eliminat totes les ocurrències dels elements en y.
(defn remove-list [ x
                    y]
    (loop [ y y
            res x]
        (if (empty? y)
            res
            (recur (rest y) (remove-element res (first y)))
        )
    )   
)


;donada a llista d’enters l, retorna dues llistes enters i senars

(defn odds-n-even [l]
    (loop [ lst l
            o []
            e []]
        (if (empty? lst)
            (list o e)
            (if (even? (first lst))
                (recur (rest lst) o (conj e (first lst)))
                (recur (rest lst) (conj o (first lst)) e)
            )
        )
    )
)

(defn prime? [n]
    (cond
        (<= n 1) false
        (= n 2) true
        (even? n) false
        :else
        (loop [div 3]
            (cond
                (> div (Math/sqrt n)) true 
                (= (mod n div) 0) false    
                :else (recur (+ div 2))
            )
        )
    )
)


;prime−divisors que retorni la llista de divisors primers d’un enter estrictament positiu.
(defn prime-divisors [n]
    (let [lp '()]
        (loop [ i 2
                lp lp
                n n]
            (if (<= n 1)
                lp
                (if (and 
                        (= (mod n i) 0)
                        (prime? i)
                    )
                    (recur i (conj lp i) (/ n i))
                    (recur (inc i) lp n)
                )
            )
        )
    )
)


;; Function to perform operations based on the operator symbol
(defn opera [x y o]
  (o x y)
)

(defn op? [c]
    (cond 
        (= c \+) true
        (= c \-) true
        (= c \*) true
        (= c \/) true
        :else false
    )
)


(defn rpf [l sk r]
    (println l sk r)
    (if (empty? l)
        r
        (if (op? (first l))
            (recur (rest l) (rest sk) (opera (first sk) r (first l))) ; pop
            (recur (rest l) (cons (first l) sk) r) ; push
        )
    )
)

(defn post-fixa [l]
    (rpf (rest l) '() (first l))
)


(println (post-fixa '(10 1 +))) ; Output: 11
(println (post-fixa '(10 1 + 2 *))) ; Output: 22

;:else (throw (Exception. "Invalid operator"))) ; Handle invalid operators
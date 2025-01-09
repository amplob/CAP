(ns llistes)

;recursive count the number of elements in a list
(defn mycount [ll
                n]
    (if (empty? ll)
        n
        (mycount (rest ll) (inc n)))
)

;recursive count the number of elements in a list
(defn my-count1 [ll]
    (mycount ll 0)
)

(defn my-count2 [l]
  (loop [   lst l
            i 0] 
    (if (empty? lst)
      i
      (recur (rest lst) (inc i)))))

(defn maximum [ l
                max]
    (if (empty? l)
        max
        (if (< max (first l))
            (maximum (rest l) (first l))
            (maximum (rest l) max)
        )
    )
)

(defn my-maximum1 [l]
    (maximum l 0)
)

(defn my-maximum2 [l]
    (loop [ lst l
            max 0] 
        (if (empty? lst)
            max
            (if (< max (first lst))
                (recur (rest lst) (first lst))
                (recur (rest lst) max)
            )
        )
    )
)

(defn myaverage     [l
                    sum
                    n]
    (if (empty? l)
        (/ sum n)
        (myaverage (rest l) (+ sum (first l)) (inc n))
    )
)

(defn average1 [l]
    (if (empty? l)
        0   
        (myaverage (rest l) (first l) 1)
    )
)

(defn average2 [l]
    (loop   [lst l
            n 0
            sum 0]
        (if (empty? lst)
            (if (= n 0)
                0
                (/ sum n)
            )
            (recur (rest lst) (inc n) (+ sum (first lst)))
        )
    )
)


(println (my-count1 '(1 2 2)))
(println (my-count2 '(1 2 2)))
(println (my-maximum1 '(4 3 1 5 4 5 2)))
(println (my-maximum2 '(4 3 1 5 4 5 2)))
(println (average1 '(1 2 3)))
(println (average2 '(1 2 3)))


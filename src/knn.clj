(ns knn)


(use 'iris :reload-all)




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

(println (de [1 0] [1 1]) )
(println (de [0 0 0] [1 1 1]) )
(println (dm [1 0] [1 1]) )
(println (dm [0 0 0] [1 1 1])  )
(println  (dh [:a :b] [:b :b]) )
(println  (dh [:a :a :a] [:b :b :b]) )



(defn classificacio [taula ex fd]
  (reduce min (map 
    (fn [t1] (fd ex t1)) 
    taula)
  )

)

(println  (classificacio iris tst-iris de) )
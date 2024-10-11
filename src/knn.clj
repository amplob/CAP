(ns knn)

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


(println (de [1 0] [1 1]) )
(println (de [0 0 0] [1 1 1]) )


(println (dm [1 0] [1 1]) )
(println (dm [0 0 0] [1 1 1])  )
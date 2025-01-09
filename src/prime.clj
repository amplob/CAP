(ns prime)

;;return if n is divisible by x
(defn div [n x] 
    (zero? 
        (mod n x)
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


(ns examen)

(defn my-take-while [f s]
    (if (empty? s) '()
        (let [[cap & cua] s]
        (if (f cap)
            (cons cap (my-take-while f cua))
            '()))))


;;(println (my-take-while even? (range 10)))

;;(println (filter even? (range 10)))

;;(println (range 10))

;;(fold funció valor llista)

(println (first '(3 4 5)))
    

(println (#(let[[cap & cua] %1] cap) '(3 4 5)))

(defn my-first [s]
    (let[[cap & cua] s] cap))

(println (my-first '(4 5 6)))

(defn my-first-f [s]
    (partial reduce even? s))

(println (my-first-f '(4 5 6)))



(defn factorial [n]
  (letfn [(fact-acc [n acc]
            (if (zero? n)
              acc
              (recur (dec n) (* n acc))))]
    (fact-acc n 1)))


(defn sum-first-n [n]
  (loop [i 1
         total 0]
    (if (> i n)
      total
      (recur (inc i) (+ total i)))))

;;En Haskell tenim una funció until que, donat un predicat p, una funció f i un valor inicial
;;v, va aplicant la funció f tal que v, f(v), f(f(v)),... fins que es satisfa el predicat. Per
;;exemple: (until #(> % 100) #(* 2 %) 1) té com a resultat 128

(defn my-until [p f x]
  (loop [v x]
    (if (p v)
      v
      (recur  (f v)))))

;;(defn my-first 
;;   (partial fold #(let[[cap & cua] %1] cap) nil))

;;(def my-first (partial fold (fn [x y] x) nil))

;;(def my-first (partial reduce (fn [x y] x) nil))


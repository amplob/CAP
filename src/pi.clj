;Gisela Borràs Zaplana
;Marc Planas Bosch

(ns pi)

(def pi4
  (map (fn [n] (/ (if (even? n) 1 -1) (+ 1 (* 2 n))))
       (iterate inc 0)))

(defn pi [epsilon]
  (loop [terms pi4
         sum 0.0]
    (let [t (first terms)]
      (if (< (Math/abs (double t)) epsilon)
        (* 4 sum)  ; pi = 4 * (suma parcial de pi/4)
        (recur (rest terms) (+ sum t))))))


(println (take 10 pi4)) ; (1 -1/3 1/5 -1/7 1/9 -1/11 1/13 -1/15 1/17 -1/19)
(println (pi 0.0001))   
(println (pi 0.00001))
(println (pi 0.000001))
(println (pi 0.0000001))

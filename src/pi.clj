;Gisela Borràs Zaplana
;Marc Planas Bosch

(ns pi)

(def pi4
  (map (fn [n] (/ (if (even? n) 1 -1) (+ 1 (* 2 n)))) ; en el divisor hi ha 1+n*2 (si comencem pel 0)
       (iterate inc 0))) ; genera una seqüència infinita començant pel 0

(defn pi [epsilon]
  (loop [seq-infinita pi4
         sum 0.0]
    (let [terme (first seq-infinita)]
      (if (< (Math/abs (double terme)) epsilon) 
        (* 4 sum)  ; hem calculat el valor aproximat de (pi/4)
        (recur (rest seq-infinita) (+ sum terme))))))


(println (take 10 pi4)) ; (1 -1/3 1/5 -1/7 1/9 -1/11 1/13 -1/15 1/17 -1/19)
(println (pi 0.0001))   
(println (pi 0.00001))
(println (pi 0.000001))
(println (pi 0.0000001))

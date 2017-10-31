(ns ru.nsu.fit.dt.sazonova.lazy-integral
    (:gen-class))
    
(defn stepsNumber
    "Returns number of steps in approximation with fixed step length."
    [x step]
    (inc (int (quot x step))))

(defn approximate
    "Returns trapezoid approximation of integral in [n * step, (n + 1) * step]."
    [function step n]
        (* 1/2 (+ (function (* n step)) (function (* (+ n 1) step))) step))

(defn approximateRange
    "Returns trapezoid approximation of integral in [from to]."
    [function from to]
        (* 1/2  (+ (function from) (function to)) (- to from)))
        
(defn approximateSumms
    [function step]
    (let [sum (map
                first
                (iterate
                    (fn 
                        [[sum n]]
                            [(+ sum (approximate function step n)) (inc n)])
                    [0. 0]))]
        (fn [n] (nth sum n))))

(defn integrate
    "Returns function that perform numerical integration by the method of trapezoids."
    [function]
        (let  [step 0.001
                approximation (approximateSumms function step)]
            (fn [x]
                (+
                (approximation (stepsNumber x step))
                (approximateRange function  (* step (stepsNumber x step)) x)))))

(defn constant
    [x]
1)
                            
(defn line
    [x]
x)
                            
(defn sin
    [x]
(Math/sin x))
                
(defn -main
        [& args]
    (println ((integrate line ) 2.))
    (let [integrate-sin (integrate sin)]
        (time (integrate-sin 100.))
        (time (integrate-sin 100.))
        (time (integrate-sin 101.))
        (time (integrate-sin 99.))))
(ns ru.nsu.fit.dt.sazonova.lazy-integral
    (:gen-class))
    
(defn stepsNumber
    "Returns number of steps in approximation with fixed step length."
    [x step]
    (quot x step))    

(defn approximate
    "Returns trapezoid approximation of integral in [n * step, (n + 1) * step]."
    [function n step]
        (* 1/2 (+ (function (* n step)) (function (* (+ n 1) step))) step))

(defn approximateRange
    "Returns trapezoid approximation of integral in [from to]."
    [function from to]
        (* 1/2  (+ (function from) (function to)) (- to from)))

(defn approximateSum
    "Returns lazy sequence of integral approximations for function and step."
    [function step]
    (map
            first
            (iterate
                (fn [[sum n]]
                    [(+ sum (approximate function n step)) (inc n)])
                [0 0.])))

(def memoizeApproximateSum
    (memoize approximateSum))

(let [step 0.001]
    (defn integrate
    "Returns function that perform numerical integration by the method of trapezoids."
    [function]
    (fn [x]
            (+
                (nth (memoizeApproximateSum function step) (stepsNumber x step))
                (approximateRange function (* step (stepsNumber x step)) x)))))

(defn constant
    [x]
    1)

(defn line
    [x]
    x)

(defn sin
    [x]
    (Math/sin x))

;(def memoizeApproximate 
;    (memoize approximateSum))

(defn -main
        [& args]
    (println ((integrate line ) 1.))
    (time ((integrate sin ) 10.))
    (time ((integrate sin ) 100.))
    (time ((integrate sin ) 101.))
    (time ((integrate sin ) 99.)))
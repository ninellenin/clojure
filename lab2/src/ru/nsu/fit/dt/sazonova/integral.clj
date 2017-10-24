(ns ru.nsu.fit.dt.sazonova.integral
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

(let [step 0.001]
    (defn integrate
    "Returns function that perform numerical integration by the method of trapezoids."
    [function, approximate]
    (fn [x]
       ( + (reduce
            (fn 
                [sum i]
                (+ sum (approximate function i step)))
            0.
            (range (stepsNumber x step)))
            (approximateRange function x (* step (stepsNumber x step))))
    )))

(defn constant
    [x]
    1)

(defn line
    [x]
    x)

(defn sin
    [x]
    (Math/sin x))

(def memoizeApproximate 
    (memoize approximate))

(defn -main
        [& args]
    (println ((integrate line  approximate) 1.))
    (time ((integrate sin  approximate) 10.))
    (time ((integrate sin memoizeApproximate) 100.))
    (time ((integrate sin memoizeApproximate) 101.))
    (time ((integrate sin memoizeApproximate) 99.)))
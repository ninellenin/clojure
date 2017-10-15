(defn stepsNumber
    [x step]
    (quot x step))    

(defn approximate
    "Returns trapezoid approximation of integral in [n * step, (n + 1) * step]."
    [function n step]
        (* 1/2 (+ (function (* n step)) (function (* (+ n 1) step))) step))

(let [step 0.001]
    (defn integrate
    "Returns function that perform numerical integration by the method of trapezoids."
    [function]
    (fn [x]
       (reduce
            (fn 
                [sum i]
                (+ sum (approximate function i step)))
            0.
            (range (stepsNumber x step)))))
    )

(defn sinCos
    [x]
    (Math/sin (Math/cos x)))

(defn constant
    [x]
    1)

(println ((integrate sinCos) 1.))
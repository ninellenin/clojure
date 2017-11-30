(ns ru.nsu.fit.dt.sazonova.bool-expressions.utils
    (:gen-class))

(defn apply-rule
    [rules argument]
    ((some (fn [rule]
            (if ((first rule) argument)
                (second rule)
                false))
        rules)
     argument))

(defn cartesian 
    [colls]
    (if (empty? colls)
      '(())
      (for [x (first colls)
            more (cartesian (rest colls))]
       (cons x more))))
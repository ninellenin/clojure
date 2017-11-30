(ns ru.nsu.fit.dt.sazonova.bool-expressions.core
  (:require [clojure.set :refer :all])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.signification])
  (:gen-class))

(defn cartesian 
  [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
        more (cartesian (rest colls))]
    (cons x more))))

(defn -main
  [& args]
    (let [expression (logic-or (variable :x) (variable :y))
        expression2 (logic-and (logic-not (variable :x)) (logic-not (variable :y)))
        expressions ((variable :x) (logic-and (variable :y) (variable :z)))
        or-parts (map expression-argument (filter logic-or? expressions))
        rest-parts (filter (fn [expression] (not (logic-or? expression))) expressions)
        expression (logic-and (variable :x) (variable :y))
        variables (list :x)
        var (first variables)
        tail (get-all-significations (rest variables))]
      ;(println var tail)
      ;(println (merge '{} {var true}))
      (println (get-all-variables (logic-not expression)))
      (println (equals? expression2 (apply logic-and (map (fn [expr] (logic-not expr)) (expression-argument expression)))))
      (println (print-expression expression))
      (println (equals? (constant true) (logic-or (variable :x) (constant true))))))
    ; (println (print-expression (apply logic-and expressions) ))
    ; (println (signification (variable :x) {:x true}))))
      ;(print-expression (apply logic-or expressions))))
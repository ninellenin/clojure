(ns ru.nsu.fit.dt.sazonova.bool-expressions.core
  (:require [clojure.set :refer :all])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.normalization])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.signification])
  (:gen-class))

(defn -main
  [& args]
  (let [expression1 (logic-and (logic-or (variable :x) (variable :y)) (variable :z) 
              (logic-or (variable :a) (variable :b)))
        expression2 (logic-not (logic-and (logic-or (logic-or (variable :x) (variable :y)) (variable :x)) 
                              (logic-or (variable :y) (variable :x))))]
      (println (print-expression expression1))
      (println (print-expression (normalize expression1)))
      (println (print-expression expression2))
      (println (print-expression (normalize expression2)))))

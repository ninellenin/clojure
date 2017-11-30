(ns ru.nsu.fit.dt.sazonova.bool-expressions.core
  (:require [clojure.set :refer :all])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.normalization])
  (:gen-class))

(defn -main
  [& args]
  (let [expression (logic-and (logic-or (variable :x) (variable :y)) (variable :z) 
              (logic-or (variable :a) (variable :b)))]
    (println (print-expression expression))
    (println (print-expression (normalize expression)))))

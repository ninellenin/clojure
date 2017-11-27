(ns ru.nsu.fit.dt.sazonova.bool-expressions.core
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
  (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
  (:gen-class))

(defn -main
  [& args]
    (let [;expressions (list (variable :x) (variable :x))
        or-parts (list (list (list :var :a) (list :var :b))
         (list (list :var :c) (list :var :d) (list :var :e)))
        fst (first or-parts)
        rst (rest or-parts)]
    (println 
      (reduce 
        (fn 
        [coll expressions]
        (map
          (fn
            [expression]
            (map
              (fn 
                [tail]
                (list tail expression))
              coll))
          expressions))
        (first or-parts)
      (rest or-parts)))))
    ;(println (logic-and (variable :x) (logic-not (variable :y))))))

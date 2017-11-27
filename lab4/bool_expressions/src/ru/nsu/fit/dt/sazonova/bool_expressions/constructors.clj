(ns ru.nsu.fit.dt.sazonova.bool-expressions.constructors
    (:require [clojure.set :refer :all])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
    (:gen-class))

(defn constant 
    "Constructor for boolean constant."
    [value]
    {:pre [(or (true? value) (false? value))]}
    (list :const value))

(defn invert-constant
    "Constructor for constant with opposite value."
    [const]
    {:pre [(constant? const)]}
    (list :const (if (= (constant-value const) false) true false)))

(defn variable 
    "Constructor for boolean variable (name must be keyword)."
    [name]
    {:pre [(keyword? name)]}
    (list :var name))
 
(declare logic-and)

(def logic-and-rules
    (list 
        [(fn [expressions] (= (count expressions) 1)) 
            (fn [expressions] (first expressions))]
        [(partial some  false-constant?) 
            (fn [expression] (constant false))]
        [(partial some true-constant?)
            (fn [expressions] (if (every? true-constant? expressions) (constant true)
                (apply logic-and (filter (fn [x] (not (true-constant? x))) expressions))))]
        [(fn [expressions] (not= (count expressions) (count (set expressions))))
            (fn [expressions] (apply logic-and (distinct expressions)))]
        ;if containts (x & not(x)) => false
        [(fn [expressions] (and (some variable? expressions)
                            (some logic-not? expressions)
                            (not (empty? (clojure.set/intersection 
                                (set (filter variable? expressions))
                                (set (map expression-argument (filter logic-not? expressions))))))))
            (fn [expessions] (constant false))]
       ; [(partial some logic-or?)
           ; (fn [expressions]
              ;  (let [or-parts (filter logic-or? expressions)
                  ;      other-parts (filter (fn [expression] (not (logic-or? expression))) expressions)]
                   ; (apply logic-or 
                      ;  (
                  ;  ))
               ; ))]
        [some?
            (fn [expressions] (list :or expressions))]))
    
(defn logic-and 
    "Constructor for logic and of two or more expressions."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    ((some 
        (fn [rule]
            (if ((first rule) expressions)
                (second rule)
                false))
            logic-and-rules)
    expressions))

(def logic-not-rules
    (list 
        [constant? invert-constant]
        [logic-not? (fn [x] (expression-argument x))]
        [variable? (fn [x] (list :not x))]))

(defn logic-not
    [expression]    
    {:pre [(expression? expression)]}
    ((some (fn [rule]
            (if ((first rule) expression)
                (second rule)
                false))
        logic-not-rules)
    expression))
    


(defn logic-or 
    [& expressions]
  ;  {:pre [(every? expression? expressions)]}
    (list ::or expressions))

(defn implication
    [premise consequence]
  ; {:pre [(expression? premise)
  ;          (expression? consequence)]}
    (list :impl premise consequence))

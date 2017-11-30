(ns ru.nsu.fit.dt.sazonova.bool-expressions.constructors
    (:require [clojure.set :refer :all])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.utils])
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
(declare logic-or)
(declare logic-not)

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
            (fn [expressions] (constant false))]
        [(partial some logic-or?)
            (fn [expressions]
                (let [or-parts (filter logic-or? expressions)
                    other-parts (filter (fn [expression] (not (logic-or? expression))) expressions)]
                    (apply logic-or 
                        (map
                        (fn 
                            [x] 
                            (apply logic-and (concat other-parts x)))
                        (cartesian (map expression-argument or-parts))))))]
        [some?
            (fn [expressions] (list :and expressions))]))

(def logic-or-rules
    (list 
        [(fn [expressions] (= (count expressions) 1)) 
            (fn [expressions] (first expressions))]
        [(partial some  true-constant?) 
                (fn [expression] (constant true))]
        [(partial some false-constant?)
            (fn [expressions] (if (every? false-constant? expressions) (constant false)
                (apply logic-or (filter (fn [x] (not (false-constant? x))) expressions))))]
        ;if containts (x | not(x)) => true
        [(fn [expressions] (and (some variable? expressions)
            (some logic-not? expressions)
            (not (empty? (clojure.set/intersection 
                (set (filter variable? expressions))
                (set (map expression-argument (filter logic-not? expressions))))))))
                (fn [expressions] (constant true))]
        [some?
            (fn [expressions] (list :or expressions))]))   

(def logic-not-rules
    (list 
        [constant? invert-constant]
        [logic-not? (fn [expression] (expression-argument expression))]
        [variable? (fn [expression] (list :not expression))]
        [logic-or? (fn [expression] (apply logic-and (map (fn [expr] (logic-not expr)) (expression-argument expression))))])) 

(defn logic-and 
    "Constructor for logic and of two or more expressions."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (apply-rule logic-and-rules expressions))

(defn logic-not
    [expression]    
    {:pre [(expression? expression)]}
    (apply-rule logic-not-rules expression))

(defn logic-or 
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (apply-rule logic-or-rules expressions))

(defn implication
    [premise consequence]
   {:pre [(expression? premise)
            (expression? consequence)]}
    (logic-or (logic-not premise) consequence))

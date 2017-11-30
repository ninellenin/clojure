(ns ru.nsu.fit.dt.sazonova.bool-expressions.normalization
    (:require [clojure.set :refer :all])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.utils])
    (:gen-class))


(declare normalize-logic-not)
(declare normalize-logic-and)
(declare normalize-logic-or)

(def logic-and-rules
    (list 
        [(fn [expressions] (= (count expressions) 1)) 
            (fn [expressions] (first expressions))]
        [(partial some  false-constant?) 
            (fn [expression] (constant false))]
        [(partial some true-constant?)
            (fn [expressions] (if (every? true-constant? expressions) (constant true)
                (apply normalize-logic-and (filter (fn [x] (not (true-constant? x))) expressions))))]
        ;containts duplicates
        [(fn [expressions] (not= (count expressions) (count (set expressions))))
            (fn [expressions] (apply normalize-logic-and (distinct expressions)))]
        ;containts (x & not(x)) => false
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
                    (apply normalize-logic-or 
                        (map
                        (fn 
                            [x] 
                            (apply normalize-logic-and (concat other-parts x)))
                        (cartesian (map expression-argument or-parts))))))]
        [some?
            (fn [expressions] (apply logic-and expressions))]))

(def logic-or-rules
    (list 
        [(fn [expressions] (= (count expressions) 1)) 
            (fn [expressions] (first expressions))]
        [(partial some  true-constant?) 
                (fn [expression] (constant true))]
        [(partial some false-constant?)
            (fn [expressions] (if (every? false-constant? expressions) (constant false)
                (apply normalize-logic-or (filter (fn [x] (not (false-constant? x))) expressions))))]
        ;containts duplicates
        [(fn [expressions] (not= (count expressions) (count (set expressions))))
            (fn [expressions] (apply normalize-logic-or (distinct expressions)))]
        ;if containts (x | not(x)) => true
        [(fn [expressions] (and (some variable? expressions)
            (some logic-not? expressions)
            (not (empty? (clojure.set/intersection 
                (set (filter variable? expressions))
                (set (map expression-argument (filter logic-not? expressions))))))))
                (fn [expressions] (constant true))]
        [some?
            (fn [expressions] (apply logic-or expressions))]))   

(def logic-not-rules
    (list 
        [constant? invert-constant]
        [variable? (fn [expression] (logic-not expression))]
        [logic-not? (fn [expression] (expression-argument expression))]
        [logic-or? (fn [expression] (apply normalize-logic-and (map (fn [expr] (normalize-logic-not expr))
             (expression-argument expression))))]
        [logic-and? (fn [expression] (apply normalize-logic-or (map (fn [expr] (normalize-logic-not expr))
             (expression-argument expression))))])) 

(defn normalize-logic-and 
    "Normalization for logic and."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (apply-rule logic-and-rules expressions))

(defn normalize-logic-not
    "Normalization for logic not."
    [expression]    
    {:pre [(expression? expression)]}
    (apply-rule logic-not-rules expression))

(defn normalize-logic-or 
    "Normalization for logic or."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (apply-rule logic-or-rules expressions))

(def normalize-rules
    (list 
        [constant? (fn [expression] expression)]
        [variable? (fn [expression] expression)]
        [logic-not? (fn [expression] (normalize-logic-not (expression-argument expression)))]
        [logic-and? (fn [expression] (apply normalize-logic-and (expression-argument expression)))]
        [logic-or? (fn [expression] (apply normalize-logic-or (expression-argument expression)))]))

(defn normalize 
    "Convert expression to disjunctive normal form."
    [expression]
    {:pre [(expression? expression)]}
    (apply-rule normalize-rules expression))
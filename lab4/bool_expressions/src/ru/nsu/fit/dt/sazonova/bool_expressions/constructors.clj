(ns ru.nsu.fit.dt.sazonova.bool-expressions.constructors
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

(defn logic-and 
    "Constructor for logic and of two or more expressions."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (list :and expressions))
    
(defn logic-not
    "Constructor for logic not of expression."
    [expression]    
    {:pre [(expression? expression)]}
    (list :not expression))

(defn logic-or 
    "Constructor for logic or of expressions."
    [& expressions]
    {:pre [(every? expression? expressions)]}
    (list :or expressions))

(defn implication
    [premise consequence]
   {:pre [(expression? premise)
            (expression? consequence)]}
    (logic-or (logic-not premise) consequence))


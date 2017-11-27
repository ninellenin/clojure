(ns ru.nsu.fit.dt.sazonova.bool-expressions.api
    (:gen-class))

(defn expression?
    "Check if parameter is expression (constant, variable, logic-and, etc.)"
    [parameter]
    (keyword? (first parameter)))

(defn expression-type
    "Get expression type"
    [expression]
    {:pre [(expression? expression)]}
    (first expression))

(defn expression-argument
    "Get the expression argument."
    [expression]
    {:pre [(expression? expression)]}
        (second expression))

(defn constant?
    "Check if the expression is constant."
    [expression]
    (= (expression-type expression) :const))

(defn constant-value
    "Get the value of constant."
    [constant]
    {:pre [(constant? constant)]}
    (expression-argument constant))

(defn true-constant?
    "Check if the expression is constant with true value."
    [expression] 
    (and (constant? expression)
        (= true (constant-value expression))))

(defn false-constant?
    "Check if the expression is constant with false value."
    [expression] 
    (and (constant? expression)
        (= false (constant-value expression))))        

(defn variable?
    [expression]
    (= (expression-type expression) :var))
        
(defn variable-name
    "Get the variable name."
    [variable]
    {:pre [(variable? variable)]}
    (expression-argument variable))   
                 
    
(defn same-variable?
    "Check if two variables are equals."
    [variable1 variable2]
    {:pre [(variable? variable1)
        (variable? variable2)]}
        (= (variable-name variable1) (variable-name variable2)))
            
(defn logic-not?
    [expression]
    (= (expression-type expression) :not))
                          
(defn logic-or? 
    [expression]
    (= (expression-type expression) :or))
                          
(defn logic-and?
    [expression]
    (= (expression-type expression) :and))


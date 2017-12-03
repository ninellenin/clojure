(ns ru.nsu.fit.dt.sazonova.bool-expressions.api
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.utils])
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
    "Check if expression is logic not."
    [expression]
    (= (expression-type expression) :not))
                          
(defn logic-or? 
    "Check if expression is logic or."
    [expression]
    (= (expression-type expression) :or))
                          
(defn logic-and?
    "Check if expression is logic and."
    [expression]
    (= (expression-type expression) :and))

(defn atom?
    "Check if expression is variable or negation of variable."
    [expression]
    (or (variable? expression) 
        (and (logic-not? expression) (variable? (expression-argument expression)))))

(defn atom-variable
    "Get variable of the atom."
    [atom]
    {:pre [(atom? atom)]}
    (if (variable? atom) (variable-name atom)
        (variable-name (expression-argument atom))))

(declare print-expression)
(declare print-nested-expression)

(def print-expression-rules
    (list 
        [constant? 
            (fn [constant] (if (constant-value constant) 1 0))]
        [variable?
            (fn [variable] (name (variable-name variable)))]
        [logic-not?
            (fn [expression] (str "~" (print-nested-expression (expression-argument expression))))]
        [logic-and?
            (fn [expression] 
                (reduce
                    (fn [result expr]
                        (str result "&" (print-nested-expression expr)))
                    (print-nested-expression (first (expression-argument expression)))
                    (rest (expression-argument expression))))]
        [logic-or?
            (fn [expression]
                (reduce 
                    (fn [result expr]
                        (str result "|" (print-nested-expression expr)))
                    (print-nested-expression (first (expression-argument expression)))
                    (rest (expression-argument expression))))]
                ))

(defn get-all-variables
    "Get all variables of expression."
    [expression]
    (if (constant? expression)
        '()
        (if (variable? expression)
            (list (variable-name expression))
            (if (logic-not? expression)
                (get-all-variables (expression-argument expression))
                    (distinct (flatten (map (fn [expr] (get-all-variables expr)) (expression-argument expression))))))))
        
(defn print-expression 
    "Printer for expression."
    [expression]
    {:pre [(expression? expression)]}
     (apply-rule print-expression-rules expression))

(defn print-nested-expression
    "Printer for brackets in nested expressions."
    [expression]
    {:pre [(expression? expression)]}
    (if (or (logic-or? expression) (logic-and? expression))
    (str "(" (print-expression expression) ")") (print-expression expression)))
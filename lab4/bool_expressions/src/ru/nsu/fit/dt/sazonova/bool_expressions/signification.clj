(ns ru.nsu.fit.dt.sazonova.bool-expressions.signification
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.api])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.constructors])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.normalization])
    (:use [ru.nsu.fit.dt.sazonova.bool-expressions.utils])
    (:gen-class))

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

(defn get-all-significations
    "Get all possible signification of input variables."
    [variables]
    (if (empty? variables)
        '({})
        (let [var (first variables)]
         (for [value (list true false)
               tail (get-all-significations (rest variables))]
             (merge tail {var value})))))

(declare significate)

(def signification-rules
    (list
        [(fn [expression]
            (constant? expression))
         (fn [expression values]
             expression)]
        [(fn [expression]
            (variable? expression))
         (fn [expression values]
             (if (contains? values (variable-name expression))
                 (constant (get values (variable-name expression)))
                 expression))]
        [(fn [expression]
            (logic-not? expression))
         (fn [expression values]
             (normalize-logic-not (significate (expression-argument expression) values)))]
        [(fn [expression]
            (logic-and? expression))
         (fn [expression values]
             (apply normalize-logic-and (map (fn [expr] (significate expr values)) (expression-argument expression))))]
        [(fn [expression]
            (logic-or? expression))
         (fn [expression values]
             (apply normalize-logic-or (map (fn [expr] (significate expr values)) (expression-argument expression))))]))
    
(defn significate
    "Returns value of signification variable values (map of variable-name: bool) in expression."
    [expression variable-values]
    ((some (fn [rule]
            (if ((first rule) expression)
                (second rule)
                false))
        signification-rules)
     expression variable-values))


(defn get-all-values
    "Get values of significations."
    [expression significations]
    (map 
        (fn [signification] 
            (significate expression signification))
     significations))
        
(defn equals?
    "Check if two expressions are equals."
    [expression1 expression2]
    (let [variables1 (set (get-all-variables expression1))
          variables2 (set (get-all-variables expression2))]
        (if (clojure.set/subset? variables1 variables2)
            (let [significations (get-all-significations variables2)]
                (= (get-all-values expression1 significations) (get-all-values expression2 significations)))
            (if (clojure.set/subset? variables2 variables1)
                (let [significations (get-all-significations variables1)]
                    (= (get-all-values expression1 significations) (get-all-values expression2 significations)))
                false))))
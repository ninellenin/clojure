(ns ru.nsu.fit.dt.sazonova.bool-expressions.api-test
    (:require [clojure.test :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.api :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.constructors :refer :all]))
        
(deftest api-test
    (testing "Testing of constant."
        (is (constant? (constant true)))
        (is (= false (constant-value (constant false))))
        (is (true-constant? (constant true)))
        (is (false-constant? (constant false))))
    (testing "Testing of variable."
        (is (variable? (variable :x))
        (is (= :x (variable-name (variable :x))))))
    (testing "Testing of logic and."
        (is (true-constant? (logic-and (constant true) (constant true))))
        (is (false-constant? (logic-and (constant true) (constant false))))
        (is (false-constant? (logic-and (constant false) (constant true))))
        (is (false-constant? (logic-and (constant false) (constant false))))
        (is (= (variable :x) (logic-and (variable :x) (constant true))))
        (is (false-constant? (logic-and (variable :x) (constant false))))
        (is (false-constant? (logic-and (variable :x) (logic-not (variable :x)))))
        (is (= (variable :x) (logic-and (variable :x) (variable :x)))))
    (testing "Testing of logic not."
        (is (false-constant? (logic-not (constant true))))
        (is (= (variable :x) (logic-not (logic-not (variable :x)))))))
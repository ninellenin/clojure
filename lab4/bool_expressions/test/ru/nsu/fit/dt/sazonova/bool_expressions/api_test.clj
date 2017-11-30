(ns ru.nsu.fit.dt.sazonova.bool-expressions.api-test
    (:require [clojure.test :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.api :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.constructors :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.signification :refer :all]))

(deftest signification-test
    (testing "Testing of signification."
        (is (true-constant? (significate (constant true) {})))
        (is (true-constant? (significate (variable :x) {:x true})))
        (is (false-constant? (significate (variable :x) {:x false})))
        (is (= (variable :x) (significate (variable :x) {:y true})))
        (is (false-constant? (significate (logic-not (variable :x)) {:x true})))
        (is (false-constant? (significate (logic-and (variable :x) (variable :y)) 
            {:x true :y false})))
        (is (true-constant? (significate (logic-or (variable :x) (variable :y)) 
            {:x true :y false})))
        (is (equals? (logic-and (variable :x) (variable :y)) (logic-and (variable :y) (variable :x))))))      
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
        (is (equals? (variable :x) (logic-and (variable :x) (constant true))))
        (is (false-constant? (logic-and (variable :x) (constant false))))
        (is (false-constant? (logic-and (variable :x) (logic-not (variable :x)))))
        (is (equals? (variable :x) (logic-and (variable :x) (variable :x)))))
    (testing "Testing of logic not."
        (is (false-constant? (logic-not (constant true))))
        (is (equals? (variable :x) (logic-not (logic-not (variable :x)))))
        (is (equals? (logic-and (logic-not (variable :x)) (logic-not (variable :y))) (logic-not (logic-or (variable :x) (variable :y))))))
    (testing "Testing of logic or."
        (is (true-constant? (logic-or (constant true) (constant false))))
        (is (true-constant? (logic-or (constant false) (constant true))))
        (is (true-constant? (logic-or (constant true) (constant false))))
        (is (false-constant? (logic-or (constant false) (constant false))))
        (is (equals? (variable :x) (logic-or (variable :x) (constant false))))
        (is (equals? (constant true) (logic-or (variable :x) (constant true)))
        (is (true-constant? (logic-or (variable :x) (logic-not (variable :x))))
        (is (equals? (variable :x) (logic-or (variable :x) (variable :x))))))))

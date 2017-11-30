(ns ru.nsu.fit.dt.sazonova.bool-expressions.api-test
    (:require [clojure.test :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.api :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.constructors :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.normalization :refer :all]
        [ru.nsu.fit.dt.sazonova.bool-expressions.signification :refer :all]))

(deftest signification-test
    (testing "Testing of signification."
        (is (equals? (constant true) (significate (constant true) {})))
        (is (equals? (constant true) (significate (variable :x) {:x true})))
        (is (equals? (constant false) (significate (variable :x) {:x false})))
        (is (equals? (variable :x) (significate (variable :x) {:y true})))
        (is (equals? (constant false) (significate (logic-not (variable :x)) {:x true})))
        (is (equals? (constant false) (significate (logic-and (variable :x) (variable :y)) {:x true :y false})))
        (is (equals? (constant true) (significate (logic-or (variable :x) (variable :y)) {:x true :y false})))
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
        (is (true-constant? (normalize (logic-and (constant true) (constant true)))))
        (is (false-constant? (normalize (logic-and (constant true) (constant false)))))
        (is (false-constant? (normalize (logic-and (constant false) (constant true)))))
        (is (false-constant? (normalize (logic-and (constant false) (constant false)))))
        (is (equals? (variable :x) (normalize (logic-and (variable :x) (constant true)))))
        (is (false-constant? (normalize (logic-and (variable :x) (constant false)))))
        (is (false-constant? (normalize (logic-and (variable :x) (logic-not (variable :x))))))
        (is (equals? (variable :x) (normalize (logic-and (variable :x) (variable :x))))))
    (testing "Testing of logic not."
        (is (false-constant? (normalize (logic-not (constant true)))))
        (is (equals? (variable :x) (normalize (logic-not (logic-not (variable :x))))))
        (is (equals? (logic-and (logic-not (variable :x)) (logic-not (variable :y))) 
            (normalize (logic-not (logic-or (variable :x) (variable :y))))))
        (is (equals? (logic-or (logic-not (variable :x)) (logic-not (variable :y))) 
            (normalize (logic-not (logic-and (variable :x) (variable :y)))))))
    (testing "Testing of logic or."
        (is (true-constant? (normalize (logic-or (constant true) (constant false)))))
        (is (true-constant? (normalize (logic-or (constant false) (constant true)))))
        (is (true-constant? (normalize (logic-or (constant true) (constant false)))))
        (is (false-constant? (normalize (logic-or (constant false) (constant false)))))
        (is (equals? (variable :x) (normalize (logic-or (variable :x) (constant false)))))
        (is (equals? (constant true) (normalize (logic-or (variable :x) (constant true))))
        (is (true-constant? (normalize (logic-or (variable :x) (logic-not (variable :x)))))
        (is (equals? (variable :x) (normalize (logic-or (variable :x) (variable :x)))))))))

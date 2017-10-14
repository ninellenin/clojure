(ns ru.nsu.fit.dt.sazonova.permutations
    (:require [clojure.test :as test]))
(defn getStrings
    "Permutation strings"
    [letters, n]
    (:pre [(> n 0)])
    (reduce
        (fn [result position]
           (flatten (map 
                (fn [i]
                    (map
                        (fn [tail] 
                            (clojure.string/join
                                [(nth result i) tail]))
                        (filter 
                            (fn [l]
                                (not=  l (str (get (nth result i) position))))
                            letters)))
                (range (count result)))))
        letters
        (range (- n 1))))
(println  (sort (getStrings ["a", "b"] 3)))
    
(test/testing "Testing permutations"
(test/is (= (getStrings [] 0) []))
(test/is (= (getStrings ["a"] 1) ["a"]))
(test/is (= (sort compare (getStrings ["a", "b"] 1)) ["a", "b"]))
(test/is (= (getStrings ["a"] 2) []))
(test/is (= (sort compare (getStrings ["a", "b"] 3)) ["aba", "bab"]))
(test/is (= (sort compare (getStrings ["a", "b", "c"] 2)) ["ab", "ac", "ba", "bc", "ca", "cb"])))
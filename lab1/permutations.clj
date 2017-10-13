(defn getStrings
    "Permutation strings"
    [letters, n]
    (reduce
        (fn [result position]
           (flatten (map 
                (fn [i]
                    (map
                        (fn [tail] 
                            (clojure.string/join
                                [tail (nth result i)]))
                        (filter 
                            (fn [l]
                                (not=  l (str (nth (nth result i) position))))
                            letters)))
                (range (count result)))))
        letters
        (range (- n 1))))
(println  (getStrings ["a", "b", "c"] 3))
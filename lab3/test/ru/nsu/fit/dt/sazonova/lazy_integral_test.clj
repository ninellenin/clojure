(ns ru.nsu.fit.dt.sazonova.lazy-integral-test
    (:require [clojure.test :refer :all]
        [ru.nsu.fit.dt.sazonova.lazy-integral :refer :all]))
        
(deftest integrate-test
  (testing "Testing of function integrate."
    (is (<= (Math/abs (- ((integrate constant) 1.) 1.)) 0.001))
    (is (<= (Math/abs (- ((integrate constant) 2.) 2.)) 0.001))
    (is (<= (Math/abs (- ((integrate line) 1.) 0.5)) 0.001))
    (is (<= (Math/abs (- ((integrate line) 3.) 4.5)) 0.001))
    ))
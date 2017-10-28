(ns ru.nsu.fit.dt.sazonova.lazy-integral-test
    (:require [clojure.test :refer :all]
        [ru.nsu.fit.dt.sazonova.lazy-integral :refer :all]))
        
(deftest integrate-test
  (testing "Testing of function integrate."
    (is (<= (Math/abs (- ((integrate constant approximate) 1.) 1.)) 0.001))
    (is (<= (Math/abs (- ((integrate constant approximate) 2.) 2.)) 0.001))
    (is (<= (Math/abs (- ((integrate line approximate) 1.) 0.5)) 0.001))
    (is (<= (Math/abs (- ((integrate line approximate) 3.) 4.5)) 0.001))
    ))
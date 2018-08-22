(ns test.node
  (:require [cljs.test :as test]
            [montoux.codec.core-test]))

(enable-console-print!)
(test/run-all-tests #"montoux\.codec.*test")

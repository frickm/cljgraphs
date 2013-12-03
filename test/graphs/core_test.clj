(ns graphs.core-test
  (:require [clojure.test :refer :all]
            [graphs.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (not= 0 1))))

(deftest test-invert-empty
  (testing "invert a single edge-list"
    (is (= (invert [:1 []]) []))))

(deftest test-invert-two-out-arrows
  (testing "invert a single edge-list"
    (is (= (invert [:1 [:2 :3]]) [[:2 :1] [:3 :1]]))))

;;; and for the transpose

(def g1 {:1 [:2 :3], :2 [:4], :3 [:4], :4 [], :5 []})
(def g2 {:1 [:2 :3], :2 [:4], :3 [], :4 [], :5 []})

(deftest test-transpose-trivial-graph
  (is (= (m-transpose {}) {})))

(deftest test-transpose-single-node
  (is (= (m-transpose {:1 '()}) {:1 '()})))

(deftest test-transpose-single-node-self-edge
  (is (= (m-transpose {:1 '(:1)}) {:1 '(:1)})))

(deftest test-transpose-twonodes
  (is (= (m-transpose {:1 '(:2) :2 '(:1)}) {:1 '(:2) :2 '(:1)}))
  )

(deftest test-transpose-threenodes
  (is (= (m-transpose {:1 '(:2 :3) :2 '(:1)}) {:1 '(:2) :2 '(:1) :3 '(:1)}))
  )

(def graph {:1 '(:2)})

;;; for the dfs and the bfs
(deftest test-simple-dfs
  (is (= (dfs {:1 '(:2)}) '(:1 :2))))

(deftest test-dfs2
  (is (= (dfs {:1 [:2 :3] :2 '(:4)}) '(:1 :2 :4 :3))))

(deftest test-simple-bfs
  (is (= (bfs {:1 '(:2)}) '(:1 :2))))

(deftest test-bfs2
  (is (= (bfs {:1 [:2 :3] :2 '(:4)}) '(:1 :2 :3 :4))))


;;; topological sorting
(deftest test-topo-sorting-empty
  (is (= (topological-sorting {}) '())))

(deftest test-topo-sorting
  (is (= (topological-sorting {:1 '(:2), :2 '()}) '(:1 :2))))

(deftest test-topo-sorting
  (is (= (topological-sorting {:1 '(:2 :3), :2 '(), :3 '()}) '(:1 :3 :2))))

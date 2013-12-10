(ns graphs.core
  (:gen-class :require [cheshire.core :refer :all])
  (:import (clojure.lang.PersistentQueue))
  )

(defstruct directed-graph
  :nodes       ; The nodes of the graph, a collection
  :neighbors)  ; A function that, given a node returns a collection
               ; neighbor nodes.

;;; note that edge-list are considered to be unordered
(defn map-map [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))

(defn invert [[node edges]]
  (map #(vector % node) edges)
  )

(defn multi-merge [cmap [k v]]
  "merges a new key-value pair into the given cmap, which maps to a collection of values"
  (let [old-value (get cmap k)]
    (assoc cmap k (conj old-value v))
    )
  )

(defn empty-graph [nodes]
  (zipmap nodes (repeat '()))
  )

(defn m-transpose [graph]
  (let [invert (fn [[node edges]] (map #(vector % node) edges))
        flat-inverted-graph (reduce concat (map invert graph))]
    (reduce multi-merge (empty-graph (keys graph)) (->> graph (map invert) (reduce concat)))
    )
  )

(require 'clojure.set)
(import '(clojure.lang PersistentQueue PersistentList IPersistentStack))

(defprotocol PushPopContainer
  "Container that allows peeking, popping and pushing of elements"
  (ipeek [this] "peek into the container")
  (ipop [this] "pop this element")
  (ipush [this x] "push x into the container")
  (ipush-coll [this coll] "push coll into the container retaining order")
  )

;; note that this works for both vectors and lists (since both of them implement stack-behaviour)
(extend-protocol PushPopContainer IPersistentStack
  (ipeek [this]
         (peek this))
  (ipop [this]
        (pop this))
  (ipush [this x]
         (conj this x))
  (ipush-coll [this coll]
              (reduce ipush this (reverse coll)))
  )

;; for the queue-behaviour we must resort to a concrete type defined in java
(extend-type PersistentQueue PushPopContainer
  (ipeek [this]
         (peek this))
  (ipop [this]
        (pop this))
  (ipush [this x]
         (conj this x))
  (ipush-coll [this coll]
              (reduce ipush this coll))
  )

(defn internal-traversal [graph nodes-todo finished] (when-let [current (ipeek nodes-todo)]
                                        (let [next-nodes (remove #(finished %) (current graph))
                                              new-todo (ipush-coll (ipop nodes-todo) next-nodes)]
                                          (cons current (internal-traversal graph new-todo (conj finished current)))
                                          )
                                        ))


(defn dfs [graph] ( let [start-node (first (keys graph))]
                    (internal-traversal graph (list start-node) #{})
                    )
  )

(defn bfs [graph] ( let [start-node (first (keys graph))]
                    (internal-traversal graph (ipush (PersistentQueue/EMPTY) start-node) #{})
                    )
  )


(defn delete-node [graph node]
  (let [target-nodes (into '() (get graph node))
        ngraph (dissoc graph node)
        node-eq (fn [n] (= n node))]
    (reduce #(remove node-eq (get %1 %2)) ngraph target-nodes)
   ))

(defn in-sorted-topo [graph in-degree-map]
  (if (not-empty graph)
    (let [sorted (sort #(- (second %1) (second %2)) in-degree-map)
          next-node (first (first sorted))
          new-degree-map (reduce #(assoc %1 %2 (dec (get in-degree-map %2))) in-degree-map (get graph next-node))]
      (cons next-node (in-sorted-topo (dissoc graph next-node) (dissoc new-degree-map next-node)))
      )
    '()
    ))


(defn sort-topo [degree-graph]
  (if (empty? degree-graph) '()
    (let [sorted (sort (fn [[k1 {left :indegree}] [k2 {right :indegree}]] (- left right)) degree-graph)
          next-node (first (first sorted))
          ;;; delete next-node form the graph and decrease the respective in-edges
          new-degree-map (reduce #(update-in %1 [%2 :indegree] dec) degree-graph (:out (get degree-graph next-node)))]
      (cons next-node (sort-topo (dissoc new-degree-map next-node)))
      )
    ))


(defn sort-topo1 [degree-graph result]
  (if (empty? degree-graph) result
    (let [sorted (sort (fn [[k1 {left :indegree}] [k2 {right :indegree}]] (- left right)) degree-graph)
          next-node (first (first sorted))
          ;;; delete next-node form the graph and decrease the respective in-edges
          new-degree-map (reduce #(update-in %1 [%2 :indegree] dec) degree-graph (:out (get degree-graph next-node)))]
      (recur (dissoc new-degree-map next-node) (cons next-node result))
      )
    )
  )


;;; node-labelled-graph - each node is associated with a set of neighbors and labels
(defn node-labelled-graph [graph]
  (let [in-nodes (map-map (fn [neigh] {:in neigh :indegree (count neigh)}) (m-transpose graph))]
    (merge-with merge in-nodes (map-map (fn [v] {:out v}) graph))
    )
  )

; topological sorting of a graph
(defn topological-sorting [graph]
  (let [tr (m-transpose graph)
        in-degree-map (map-map #(count %) tr)]
    (in-sorted-topo graph in-degree-map))
  )

(def g {:1 '(:2 :3), :2 '(:4), :3 '(:2), :4 '(:5)})

(def t (node-labelled-graph g))
t
(sort-topo (node-labelled-graph g))
(sort-topo1 (node-labelled-graph g) '())

(update-in t [:3 :indegree] dec)

(merge-with concat {:a [10 20] :b [4]} {:b [20]})
(merge-with merge {:a {:e 10 :f 20} :b {:e 4}} {:b {:t 20}})

(node-labelled-graph g)

(defn update-min [map [key val]]
  (update-in map [key] (fnil #(min % val) Long/MAX_VALUE)))

;;; diijkstra algorithm - input is expected to be a graph with edge-weights
(defn distance-to-internal [graph todo-list finished]
  (let [[current-node dist] (ipeek todo-list)
        new-nodes (get graph current-node)
        new-finished (reduce update-min finished (map (fn [x] [x (inc dist)]) new-nodes))
        ]
    (if (= finished new-finished) finished
                                  (distance-to-internal graph
                                                        (ipush-coll (ipop todo-list)
                                                                    (map (fn [x] [x (inc dist)]) new-nodes))
                                                        new-finished))
    ))

(defn distance-to [graph start]
  (distance-to-internal graph (ipush PersistentQueue/EMPTY [start 0]) {start 0}))

(reduce update-min {} (map (fn [x] [x (inc 0)]) '(:a :b)))
(update-min {} (first (map (fn [x] [x (inc 0)]) '(:a :b))))
(distance-to g :1)

;;; read some random jsons
(require '[clojure.java.io :as io])
(require '[cheshire.core :refer :all])

(with-open [rdr (io/reader "/home/frick/Desktop/testpatienten/jsons/1157243")]
  (doseq [line (line-seq rdr)]
    (println line)
    )
  )

(def js (slurp "/home/frick/Desktop/testpatienten/jsons/1157243"))

(def js1 (parse-stream (io/reader "/home/frick/Desktop/testpatienten/jsons/109865")))

(get js1 "patientId")
(keys js1)
(keys (get-in js1 ["caringFacts"]))
(-> js1 (get-in ["caringFacts" "lab"]) count)
(macroexpand-1 '(-> js1 (get-in ["caringFacts"])))

(def sarah (atom {:age 10}))
(defn hello-watch [key id old new]
  (println key old "==>" new))

(add-watch sarah :one hello-watch)
(add-watch sarah :two hello-watch)
(swap! sarah update-in [:age] inc)


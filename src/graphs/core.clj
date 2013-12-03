(ns graphs.core
  (:gen-class))

(import 'clojure.lang.PersistentQueue)

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
(extend-type clojure.lang.PersistentQueue PushPopContainer
  (ipeek [this]
         (peek this))
  (ipop [this]
        (pop this))
  (ipush [this x]
         (conj this x))
  (ipush-coll [this coll]
              (reduce ipush this coll))
  )


(defn remove-set [coll st] (remove #(get st %) coll))


(defn internal-traversal [graph nodes-todo finished] (when-let [current (ipeek nodes-todo)]
                                        (let [next-nodes (remove-set (current graph) finished)
                                              new-todo (ipush-coll (ipop nodes-todo) next-nodes)]
                                          (cons current (internal-traversal graph new-todo (conj finished current)))
                                          )
                                        ))


(defn dfs [graph] ( let [start-node (first (keys graph))]
                    (internal-traversal graph (list start-node) [])
                    )
  )

(defn bfs [graph] ( let [start-node (first (keys graph))]
                    (internal-traversal graph (ipush (clojure.lang.PersistentQueue/EMPTY) start-node) [])
                    )
  )


(defn delete-node [graph node]
  (let [target-nodes (into '() (get graph node))
        ngraph (dissoc graph node)
        node-eq (fn [n] (= n node))]
    (reduce #(remove node-eq (get %1 %2)) ngraph target-nodes)
   ))

(def g {:1 '(:2 :3), :2 '(), :3 '(:2)})
(delete-node g :3)


(defn in-sorted-topo [graph in-degree-map]
  (if (not-empty graph)
    (let [sorted (sort #(- (second %1) (second %2)) in-degree-map)
          next-node (first (first sorted))
          new-degree-map (reduce #(assoc %1 %2 (dec (get in-degree-map %2))) in-degree-map (get graph next-node))]
      (cons next-node (in-sorted-topo (dissoc graph next-node) (dissoc new-degree-map next-node)))
      )
    '()
    ))


; topological sorting of a graph
(defn topological-sorting [graph]
  (let [tr (m-transpose graph)
        in-degree-map (map-map #(count %) tr)]
    (in-sorted-topo graph in-degree-map))
  )

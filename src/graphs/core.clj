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

(defn m-transpose [graph]
  (let [invert (fn [[node edges]] (map #(vector % node) edges))
        flat-inverted-graph (reduce concat (map invert graph))]
    (reduce multi-merge {} (->> graph (map invert) (reduce concat)))
    )
  )

(m-transpose {:1 '(:2)})

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


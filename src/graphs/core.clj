(ns graphs.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))




(defn map-map [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))

(defn invert [[node edges]]
  (map #(vector % node) edges)
  )

(defn transpose [graph] (let [inverted (reduce concat (map invert graph))
                              grouped (group-by first inverted)]
                          (map-map #(vec (map last %)) grouped)
                          )
  )

(defn multi-merge [cmap [k v]]
  "merges a new key-value pair into the given cmap, which maps to a collection of values"
  (let [old-value (get cmap k)]
    (assoc cmap k (conj old-value v))
    )
  )

(defn m-transpose [graph]
  (let [flat-inverted-graph (reduce concat (map invert graph))]
    (reduce multi-merge {} (->> graph (map invert) (reduce concat)))
    )

  )

;; dfs
(defn acyclic-dfs [graph start] (cons start
                              (->> (start graph) (map #(acyclic-dfs graph %)) (flatten)))
  )

(require 'clojure.set)
(import '(clojure.lang PersistentQueue PersistentList IPersistentStack))
;; dfss

(defprotocol PushPopContainer
  "Container that allows peeking, popping and pushing of elements"
  (ipeek [this] "peek into the container")
  (ipop [this] "pop this element")
  (ipush [this x] "push x into the container"))

;; note that this works for both vectors and lists (since both of them implement stack-behaviour)
(extend-protocol PushPopContainer IPersistentStack
  (ipeek [this]
         (peek this))
  (ipop [this]
        (pop this))
  (ipush [this x]
         (conj this x))
  )

;; for the queue-behaviour we must resort to a concrete type defined in java
(extend-type clojure.lang.PersistentQueue PushPopContainer
  (ipeek [this]
         (peek this))
  (ipop [this]
        (pop this))
  (ipush [this x]
         (conj this x))
  )


(def e PersistentQueue/EMPTY)
(def twoels (conj (conj e 10) 20))

(-> twoels (conj 10) (conj 20))

(peek (pop twoels))
(peek twoels)

(peek (into e '(1 2 3 4 5)))

(def x '(1 2 3 4))
(ipeek x)
(ipop x)
(ipush x 6)
(ipop (ipush x 6))
(ipeek (ipush x 6))

(def xx [1 2 3 4])
(ipeek x)
(peek (ipush (into e [1 2 3 4]) 10))


x
(ipop (ipush '(12 34) 6))


(def e clojure.lang.PersistentQueue/EMPTY)
(peek e)
(.cons e 1)
(def twomem (.cons (.cons e 1) 2))
(peek twomem)
(peek (pop twomem))

(defn remove-set [coll st] (remove #(get st %) coll))

;; issue is backtracking (resets the visisted set)
(defn acyclic-dfss [graph start visited] (let [new-nodes (start graph)]
                                   (println "nodes: " new-nodes "   visisted: " visited)
                                   (cons start
                                         (->> (remove-set new-nodes visited) (map #(acyclic-dfss graph % (clojure.set/union visited new-nodes))) (flatten)))
                                   )
  )

(import 'clojure.lang.PersistentQueue)

;; looping version
;(defn dfs [graph start] (dfs graph [start] []))
(defn dfs [graph nodes-todo finished] (when-let [current (first nodes-todo)]
                                        (let [next (remove-set (current graph) finished)
                                              new-todo (concat next (rest nodes-todo))]
                                          ;;                                           (println "(current, todo, new-todo)" current nodes-todo new-todo)
                                          ;;                                           (println "finished:" finished)
                                          ;;                                           (println "next-recur: " (cons current (dfs graph new-todo (conj finished current))))

                                          (cons current (dfs graph new-todo (conj finished current)))
                                          )
                                        ))

(defn bfs [graph nodes-todo finished] (when-let [current (first nodes-todo)]
                                        (let [next (remove-set (current graph) finished)
                                              new-todo (concat (rest nodes-todo) next)]

                                          (cons current (bfs graph new-todo (conj finished current)))
                                          )
                                        ))

(cons :2 nil)
(conj #{} :3)
(rest (concat '(1 2 3) '(4 5 6)))
(peek [:1])
(peek [:2 :3])
(conj [] [])
(conj #{1} 3)

(cons :4 nil)
(cons :4 ())

(first [1 2 3])
(first '())

(clojure.set/union #{1 2} #{})
(set [:1 :2])
(if nil 0 1)
(if 1 0 1)

(get #{1 2 3} 5)
(remove-set '(1 2 3 5) #{4 3})

(clojure.set/difference (set '(2 3 4 5)) #{1})
(set '(12 23))

(defn bla [x y z] (+ x y z))
(bla 1 2 3)

(def x (->> [1 2 3 4] (map #(+ % 1))))
x


(->> '(1 2) (fn [x] (+ x 1)))

(map #(+ % 1) [1 2 3 4] )

(cons 3 [1 2])


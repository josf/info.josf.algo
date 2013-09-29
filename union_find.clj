(ns info.josf.algo.union-find)


;; quick-find
(defn init-conn [n] (vec (range n)))
(defn qf-union [conns p q]
  (let [p-component (nth conns p)
        q-component (nth conns q)]
    (replace {q-component p-component} conns)))

(defn connected? [conns p q]
  (= (nth conns p) (nth conns q)))

;; quick union
;;


(defn qu-root [conns x]
  (let [root-candidate (nth conns x)]
   (if (= root-candidate x)
     x
     (recur conns root-candidate))))

(defn qu-union [conns p q]
  (assoc conns (qu-root conns q) (qu-root conns p)))

(defn qu-connected? [conns p q]
  (= (qu-root conns p) (qu-root conns q)))


;; weighted quick union
(defn quw-init-conn [n]
  (vec (for [x (range n)] [x 1])))

(defn quw-root [conns x]
  (let [[root-candidate weight] (nth conns x)]
    (if (= root-candidate x)
      [root-candidate weight]
      (recur conns root-candidate))))

(defn quw-union [conns p q]
  (let [[p-root p-weight] (quw-root conns p)
        [q-root q-weight] (quw-root conns q)]
    (if (> p-weight q-weight)
      (assoc conns q-root [p-root 0]
             p-root [p-root (+ p-weight q-weight)])

      (assoc conns p-root [q-root 0]
             q-root [q-root (+ p-weight q-weight)]))))

(defn quw-connected? [conns p q]
  (let [[_ p-root] (quw-root conns p)
        [_ q-root] (quw-root conns q)]
    (= q-root p-root)))

(defn path-crush [conns p new-root]
  (let [[_ weight] (nth conns p)]
    (assoc conns p [new-root weight])))

;; with path compression
(defn qupc-root
  ([conns x] (qupc-root x nil))
  ([conns x prev]
     (let [[root-candidate weight] (nth conns x)
           conn-crush (when prev (path-crush conns prev root-candidate))]
       (if (= root-candidate x)
         [root-candidate weight (or conn-crush conns)]
         (recur  conns root-candidate x)))))


(defn qupc-union [conns p q]
  (let [[p-root p-weight p-conn] (qupc-root conns p)
        [q-root q-weight q-conn] (qupc-root p-conn q)]
    (if (> p-weight q-weight)
      (assoc q-conn q-root [p-root 0]
             p-root [p-root (+ p-weight q-weight)])
      (assoc q-conn p-root [q-root 0]
             q-root [q-root (+ p-weight q-weight)]))))



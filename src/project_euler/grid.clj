;; Define data structures and functions for a grid

(ns project-euler.grid
  :require [aysylu/loom :as loom])

(defn seq->grid
  "Convert a seq of seqs to a grid"
  [s]
  (reduce
   #(assoc-in %1 [(first %2) (second %2)] {:val (last %2)})
   {}
   (for [row (range (count s)),
         col (range (count (nth s row)))]
     [row col (nth (nth s row) col)])))

(defn right
  "Find element to the right of row,col in grid"
  [{:keys [grid, row, col]}]
  {:grid grid,
   :row row,
   :col (inc col)})

(defn left
  "Find element to the left of row,col in grid"
  [{:keys [grid, row, col]}]
  {:grid grid,
   :row row,
   :col (dec col)})

(defn down
  "Find element below row,col in grid"
  [{:keys [grid, row, col]}]
  {:grid grid,
   :row (inc row),
   :col col})

(defn up
  "Find element above row,col in grid"
  [{:keys [grid, row, col]}]
  {:grid grid,
   :row (dec row),
   :col col})

(def down-right (comp down right))
(def down-left (comp down left))
(def up-right (comp up right))
(def up-left (comp up left))

(defn value
  "Return the value of grid at row,col"
  [{:keys [grid, row, col]}]
  (get-in grid [row col] :invalid))

(defn valid?
  "Determine if the combination of row,col is valid for grid"
  [{:keys [grid, row, col] :as args}]
  (not= :invalid (value args)))

(defn valid-moves
  "Determine which allowed moves for a cell are valid"
  [{:keys [grid, row, col] :as args} allowed-moves]
  (filter valid? (map #(% args) allowed-moves)))

(defn grid->digraph
  "Convert a grid to a directed graph with only allowed-moves"
  [grid allowed-moves]
  )

(defn already-visited?
  "Determine if the cell has already been visited"
  [{:keys [grid, row, col] :as cell} path]
  (if (empty? (filter #(= % cell) path))
    false
    true))

(defn same-cell?
  "Determine if two cells are the same"
  [c1 c2]
  (and
   (= (:row c1) (:row c2))
   (= (:col c1) (:col c2))))

(defn paths
  "Determine paths from start to end using allowed moves"
  [{:keys [grid, start, end, allowed-moves, current-path]
    :or {current-path []}
    :as args}]
  (cond
    (same-cell? start end) (assoc args
                                  :paths (conj
                                          current-path start))
    (empty? current-path)
    (for [move (valid-moves (assoc start :grid grid) allowed-moves)]
      (paths (assoc args
                    :start {:row (:row move), :col (:col move)}
                    :current-path
                    (conj current-path start))))
    :else (let [next-moves (remove
                            #(already-visited?
                              % current-path)
                            (valid-moves
                             start allowed-moves))]
            (for [move next-moves]
              (paths (assoc args
                             :start move
                             :current-path (conj
                                            current-path
                                            move)))))))

(comment (defn paths
   "Determine paths from start to end using allowed moves"
   [grid start end allowed-moves]
   (loop [cell {:grid grid,
                :row (:row start),
                :col (:col start)},
          complete-paths #{},
          current-path [],
          next-moves (remove #(already-visited? % current-path)
                             (valid-moves cell allowed-moves)),
          possible-paths (remove
                          #(contains? complete-paths %)
                          (for [move next-moves]
                            (conj current-path move)))]
     (cond
       (empty? possible-paths) 
       {:grid grid,
        :start start,
        :end end,
        :paths complete-paths}
       (same-cell? cell end) (recur
                              (assoc start :grid grid)
                              (conj complete-paths current-path)
                              3 4 5)
       :else (map
              (fn [path] (recur
                          (last path)
                          (complete-paths)
                          (path)
                          (remove #(already-visited?
                                    % current-path)
                                  (valid-moves
                                   (last path) allowed-moves))
                          (remove
                           #(contains? complete-paths %)
                           (for [move next-moves]
                             (conj current-path move))))
                ))))))

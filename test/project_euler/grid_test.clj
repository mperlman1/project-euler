(ns project-euler.grid-test
  (:use expectations)
  (:require [project-euler.grid :as grid]))

(def s [[0 1 2]
        [3 4 5]])
(def grid (grid/seq->grid s))
(def start {:grid grid, :row 0, :col 0})

(def lattice [[1 2 3]
              [4 5 6]
              [7 8 9]])
(def lattice-grid (grid/seq->grid lattice))
(def lattice-start {:row 0, :col 0})
(def lattice-end {:row 2, :col 2})
(def lattice-paths
  {:grid lattice-grid,
   :start {:row 0, :col 0},
   :end {:row 2, :col 2},
   :paths #{[{:row 0, :col 0},
             {:row 0, :col 1},
             {:row 0, :col 2},
             {:row 1, :col 2},
             {:row 2, :col 2}],
            [{:row 0, :col 0},
             {:row 0, :col 1},
             {:row 1, :col 1},
             {:row 1, :col 2},
             {:row 2, :col 2}],
            [{:row 0, :col 0},
             {:row 0, :col 1},
             {:row 1, :col 1},
             {:row 2, :col 1},
             {:row 2, :col 2}],
            [{:row 0, :col 0},
             {:row 1, :col 0},
             {:row 1, :col 1},
             {:row 1, :col 2},
             {:row 2, :col 2}],
            [{:row 0, :col 0},
             {:row 1, :col 0},
             {:row 1, :col 1},
             {:row 2, :col 1},
             {:row 2, :col 2}],
            [{:row 0, :col 0},
             {:row 1, :col 0},
             {:row 2, :col 0},
             {:row 2, :col 1},
             {:row 2, :col 2}]}})

(expect
 {0
  {0 {:val 0},
   1 {:val 1},
   2 {:val 2}},
  1
  {0 {:val 3},
   1 {:val 4},
   2 {:val 5}}}
 (grid/seq->grid s))

(expect
 {:grid grid,
  :row 0,
  :col 1}
 (grid/right start))

(expect
 {:grid grid,
  :row 0,
  :col -1}
 (grid/left start))

(expect
 {:grid grid,
  :row 1,
  :col 0}
 (grid/down start))

(expect
 {:grid grid,
  :row -1,
  :col 0}
 (grid/up start))

(expect
 {:grid grid,
  :row 1,
  :col 1}
 (grid/down-right start))

(expect
 {:grid grid,
  :row 1,
  :col -1}
 (grid/down-left start))

(expect
 {:grid grid,
  :row -1,
  :col 1}
 (grid/up-right start))

(expect
 {:grid grid,
  :row -1,
  :col -1}
 (grid/up-left start))

(expect
 :invalid
 (grid/value (grid/up-left start)))

(expect
 {:val 4}
 (grid/value (grid/down-right start)))

(expect
 false
 (grid/valid? (grid/up-left start)))

(expect
 true
 (grid/valid? (grid/down-right start)))

(expect
 #{{:grid grid,
    :row 0,
    :col 1},
   {:grid grid,
    :row 1,
    :col 0}}
 (apply hash-set
        (grid/valid-moves start
                          [grid/right,
                           grid/left,
                           grid/down,
                           grid/up])))

(expect
 true
 (grid/already-visited? lattice-start [lattice-start lattice-end]))

(expect
 false
 (grid/already-visited? lattice-start [lattice-end]))

(expect
 true
 (grid/same-cell? lattice-start lattice-start))

(expect
 false
 (grid/same-cell? lattice-start lattice-end))

 (expect
  lattice-paths
  (grid/paths
   {:grid lattice-grid,
    :start lattice-start,
    :end lattice-end,
    :allowed-moves [grid/right grid/down]}))

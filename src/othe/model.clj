(ns othe.model)

;; Size of each dimension
(def b-size 8)

;; Position of the first cell
(def first-pos 0)

;; Position of the last cell
(def last-pos (* b-size b-size))

;; List of all possible positions
(def all-pos (range first-pos last-pos))

;; Column indexes
(def first-col 0)
(def last-col b-size)

;; Row indexes
(def first-row 0)
(def last-row b-size)

;; Position and index mutual conversion
(defn col-from-pos [pos] (mod pos b-size))
(defn row-from-pos [pos] (quot pos b-size))
(defn pos-from-rowcol [r c]
  (+ (* r b-size) c))

;; Collection of all possible directions
(def dirs #{:n :ne :e :se :s :sw :w :nw})

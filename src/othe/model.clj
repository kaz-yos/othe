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

;; Mutable board status (ref)
(def board (ref []))

;; Mutable player order
(def player (ref nil))

;; Function to obtain neighboring cell positions
(def successor
  (let [north (fn [pos (- pos b-size)]) ; To go up, subtract number of columns
        east  inc                       ; To go right, add one
        south (fn [pos (- pos b-size)]) ; To go down, add number of columns
        west  dec]

    ;; Define functions as map
    {:n  north
     :ne (comp north east)
     :e  east
     :se (comp south east)
     :s  south
     :sw (comp south west)
     :w  west
     :nw (comp north west)}))

;; Define a map of functions to detect wrapping at the end
(def not-wrapped?
  (let [east? (fn [pos] (> (col-from-pos pos) first-col))       ; index should be > 0
        west? (fn [pos] (< (col-from-pos pos) (dec last-col)))] ; index should be < 7 (8th)

    {:n  identity
     :ne east?
     :e  east?
     :se east?
     :s  identity
     :sw west?
     :w  west?
     :nw west?}))

;; Define a function to test
(defn- in-board? [pos]
  (and (>= pos first-pos)       ; It should be >= pos
       (<  pos last-pos)))      ; It should be < (* b-size b-size)

;; Function to obtain posline
(defn- posline-for-dir
  "Cells available from pos in dir direction"
  [pos dir]
  (let [suc    (successor    dir)       ; Function to pick neighboring cell position in dir (map as function for a key)
        nwrap? (not-wrapped? dir)]      ; Function to check wrapping in dir (map as function for a key)
    (take-while                         ; Take while within valid range
     (fn [pos]
       (and (nwrap?    pos)
            (in-board? pos)))
     (iterate suc (suc pos)))))         ; successively take the neighboring cells

;; Miscellaneous functions
(defn- free? [brd pos] (= (brd pos) :free))
(defn- self? [brd pos bw]
  (and (not (free? brd pos))
       (= (brd pos) bw)))
(defn- opponent? [brd pos bw]
  (and (not free? brd pos)
       (not= (brd pos) bw)))
(defn- all-poslines
  "Return the sequence of poslines for all dirs at a given pos"
  [pos]
  (filter not-empty
          (for [dir dirs]                       ; Non-argument access to a global variable dirs
            (posline-for-dir pos dir))))

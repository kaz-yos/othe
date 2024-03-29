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
  (let [north (fn [pos] (- pos b-size)) ; To go up, subtract number of columns
        east  inc                       ; To go right, add one
        south (fn [pos] (+ pos b-size)) ; To go down, add number of columns
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
  (and (not (free? brd pos))
       (not= (brd pos) bw)))
(defn- all-poslines
  "Return the sequence of poslines for all dirs at a given pos"
  [pos]
  (filter not-empty
          (for [dir dirs]                       ; Non-argument access to a global variable dirs
            (posline-for-dir pos dir))))

;; Check if a new stone can be placed at pos in a given posline
(defn- clamping?
  "If the posline is usable for bw"
  [brd posline bw]
  (and
   (opponent? brd (first posline) bw) ; check if the neighboring cell is opponent cell
   (if-let
       ;; Assign the first pos not opponent in a given dir except the very first cell
       [fst (first (filter (fn [pos] (not (opponent? brd pos bw))) (rest posline)))]
     ;; Return this if fst is true
     (self? brd fst bw)
     ;; Return nil if fst is nil (no non-opponent cells in that dir)
     nil)))

;; Check if a given cell is playable cell (use clamping? to all poslines)
(defn- playable?
  "Check if pos is a playable cell for bw."
  [brd pos bw]
  (and
   (free? brd pos)
   (some                                ; Check all poslines for at least one clamping? true posline
    (fn [pl] (clamping? brd pl bw))
    (all-poslines pos))))

;; Initialization function
(def initial-oprs
  "opr map for the initial status"
  (let [cntr (dec (quot b-size 2))
        pos  (pos-from-rowcol cntr cntr)]
    {pos :b
     ((successor :se) pos) :b
     ((successor :e)  pos) :w
     ((successor :s)  pos) :w}))

;; A closure to create a board
(defn- board-manipulator
  "Closure to create a board based on opr map"
  [oprs]
  (fn [pos st] (if-let [s (oprs pos)]
                 s
                 st)))

;; A function to return the manipulated board
(defn- manipulated-board
  "New status of the board after manipulator"
  [brd manip]
  (vec (map-indexed manip brd)))

;; Initial status
(let
    [blank (vec (repeat (- last-pos first-pos) :free))
     manip (board-manipulator initial-oprs)]
  (manipulated-board blank manip))

;; 1 move
(defn- make-oprs
  "Calculate opr for bw for a given posline"
  [brd posline bw]
  (reduce (fn [m pos] (assoc m pos bw)) {}
          (take-while
           (fn [pos] (opponent? brd pos bw))
           posline)))

(defn- make-all-oprs
  "Calculate opr for all poslines for bw"
  [brd pos bw]
  (apply merge  ; Combine all operations for all poslines
         (cons {pos bw} ; Add pos and bw for the operation
               (for [posline
                     (filter    ; Filter poslines for clamping? true ones
                      (fn [pos] (clamping? brd pos bw))
                      (all-poslines pos))]
                 (make-oprs brd posline bw))))) ; Create operation for each posline


;; Initialize a new game (Called from core for its side effects)
(defn init-game
  "Start a new game"
  [ob]
  (dosync       ; atomic manipulation of refrence objects
   (let [blank
         (vec
          (repeat (- last-pos first-pos) :free))
         manip
         (board-manipulator initial-oprs)]
     ;; Set the board status to the initial status
     (ref-set board
              (manipulated-board blank manip)))
   ;; Set the initial player to :b
   (ref-set player :b)
   (def observer ob))
  (observer))

;; Play move to update the board and player
(defn- opponent [bw] (if (= bw :b) :w :b))
(defn- has-pos-to-play?
  "Check if there is any available cells for bw"
  [brd bw]
  (not-empty
   (filter
    (fn [pos] (playable? brd pos bw))
    all-pos)))

;; Skip a player if not playable
(defn- next-player
  "Decide the next player."
  [bw brd]
  (let [nbw (opponent bw)]
    (if (has-pos-to-play? brd nbw) nbw bw)))

;; Actual playing function (user function)
(defn play-move
  "Play for a pos"
  [pos]
  (dosync
   (if (not (playable? @board pos @player))     ; Get values from reference objects.
     ;; if not playable
     (observer :err)
     ;; if playable
     (do
       (let
           [manip
            (board-manipulator
             (make-all-oprs @board pos @player))]
         (alter board manipulated-board manip))
       (alter player next-player @board)
       (observer)))))

;; API function to obtain the model status
(defn- retrieve-game-state
  "Give :b if black is playing, :w if white is playing, :over if game over"
  []
  (let [brd @board
        bw  @player]
    ;; If no free cell is available
    (if (empty?
         (filter
          (fn [pos] (free? brd pos))
          all-pos))
      ;; Return over
      :over
      ;; Otherwise return the player
      bw)))

;; Occupancy checker
(defn- occupancy
  "How many cells bw have"
  [brd bw]
  (count
   (filter
    (fn [pos] (= (brd pos) bw))
    all-pos)))

;; Game status checkers
(defn is-game-over? []
  (= (retrieve-game-state) :over))
(defn is-black-turn? []
  (= (retrieve-game-state) :b))
(defn count-blacks [] (occupancy @board :b))
(defn count-whites [] (occupancy @board :w))
(defn retrieve-board [] @board)



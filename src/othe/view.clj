(ns othe.view
  (:use
   othe.model
   [clojure.string :only (join)]))


;; Names for columns
(def code-a 97)
(def code-curly 123)
(def col-headers
  (take b-size
        (map (comp str char)
             (range code-a code-curly))))
(def col-header-str
  (str " " (join " " col-headers)))

;; Convert to string to describe the status of cells (private)
(defn- st-str
  "String to describe the status of cells"
  [st]
  (cond
   (= st :b) "x"
   (= st :w) "o"
   :else     " "))

;; Renders each row
(defn- board-strs
  "String sequence including all lines"
  [brd]
  ;; Separate into each row
  (for [row (partition b-size brd)]
    ;; Add a space
    (join " " (map st-str row))))

;; Add row number
(defn- board-strs-with-row
  "Add row number"
  [brd]
  (map str                                      ; map str to the following lists
       (range (inc first-row) (inc last-row))   ; list 1: row numbers
       (repeat b-size " ")                      ; list 2: spaces
       (board-strs brd)))                       ; list 3: board status in strings

;; Miscellaneous components
(def separator (join (repeat 50 \-)))

(def- score-str
  "String for score"
  [bw ws]
  (let [s (str "BLACK(x):" bs ", WHITE(o):" ws)]
    (format "%50s" s)))

(defn- winner-str
  "String for the winner"
  [bs ws]
  (cond
   (> bs ws) "Winner is BLACK. Congratulations!"
   (> ws bs) "Yea, WHITE, won!!!"
   :else     "It's a draw game."))

;; Function to redraw the board
(defn- redraw-board
  "Show the board"
  []
  (println col-header-str)
  (dorun
   (map println
        (board-strs-with-row (retrieve-board)))))



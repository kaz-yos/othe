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

;;
(defn on-state-changed
  "Handler for model change"
  [& e]
  (if e
    (print "You can't move there. Input again: ")
    (let [bs (count-blacks)
          ws (count-whites)]
      (println separator)
      (println (score-str bs ws))
      (redraw-board)
      (when (is-game-over?)
        (println (str "GAME OVER: "
                      (winner-str bs ws)))
        (command-handler [:exit])))))

;;
(defn init-view
  "Initialize view. Handler is the user command handler"
  [handler]
  (println "Welcome to the Battle Zone!")
  (println "'x' is Black and 'o' is White.")
  (println "Input the column name first, like 'a1' or b2'")
  (println "Just hit Enter to exit.")
  (def command-handler handler))

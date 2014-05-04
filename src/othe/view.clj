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

(defn- score-str
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

;; Check for user input
(defn- view-thread
  "Thread to check for user inputs"
  []
  (loop [pos (wait-for-cmd)]
    (when pos
      (command-handler [:move pos])
      (recur (wait-for-cmd))))
  (command-handler [:exit]))

;; Start user interaction
(defn start-ui
  "Start user interaction"
  []
  (.start (Thread. view-thread)))

;; Check for abnormal commands
(defn- wait-for-cmd
  "Wait for user input, and return nil or pos"
  []
  (loop [line (read-cmd)]
    (if (empty? line)
      (println "Exiting...")
      (if-let [pos (pos-from-line line)]
        pos
        (do
          (print "Input should be like a1 or b2. Or Enter to exit: ")
          (flush)
          (recur (read-cmd)))))))

;; Read from the stdin
(defn- read-cmd
  "Read command from stdin"
  []
  (print (if (is-black-turn?)
           "It's BLACK's turn: "
           "Hey WHITE, your turn: "))
  (flush)
  (read-line))

;; pos-from-line
(defn- col-from-line
  "Read column from the user input"
  [line]
  (.indexOf col-headers (subs line 0 1)))

(defn- row-from-line
  "Read row from the user input"
  [line]
  (dec (read-string (subs line 1))))

(defn- pos-from-line
  "Read pos from the user input, if invalid, return nil"
  [line]
  (when (re-find #"^[a-h][1-8]$" line)
    (let [r (row-from-line line)
          c (col-from-line line)]
      (pos-from-rowcol r c))))

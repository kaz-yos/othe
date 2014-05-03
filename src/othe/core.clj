(ns othe.core
  (:use
   othe.view
   othe.model))

(defn on-command
  "Handles commands received from the view"
  [cmdline]
  (let [cmd (first  cmdline)
        pos (second cmdline)]
    (cond
     (= cmd :move) (play-move pos)
     (= cmd :exit) (System/exit 0)
     :else nil)))

(defn -main
  "Entry point"
  [& args]
  (init-view on-command)
  (init-game on-state-changed)
  (start-ui))

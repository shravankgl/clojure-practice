(ns clojure-2408.game)

(defn init-game [width win-num]
  "Initialize the game"
  (def width width)
  (def total-cells (* width width))
  (def win-num win-num)
  (def game-board
    (let [zero-board (vec (repeat (* total-cells) 0))] 
      (add-cell zero-board))))

(defn add-cell [game-board]
  "add a random value in random 0 value cell"
  (let [random-cell (pick-random-empty-cell game-board)]
    (assoc game-board random-cell (pick-rand-value))))

(defn pick-random-empty-cell [game-board]
  "get a random 0 value cell"
  (rand-nth (get-zero-indicies game-board)))

(defn pick-rand-value []
  "pick a value between 2(75% probability) and 4(25% probability) "
  (rand-nth [2 2 2 4]))

(defn get-zero-indicies [game-board]
  "get list of all indicies containing value 0"
  (map first
       (filter #(= (second %) 0)
               (map-indexed vector game-board))))

(defn draw-board [game-board]
  "draw the board"
  (doseq [row  (get-rows game-board)] (println row)))


(defn get-rows [game-board]
  (partition-all 4 game-board))


(defn get-columns [game-board]
  (list
   (vec (take-nth 4 game-board))
   (vec (take-nth 4 (drop 1 game-board)))
   (vec (take-nth 4 (drop 2 game-board)))
   (vec (take-nth 4 (drop 3 game-board)))))


(defn reverse-rows-or-columns [rows-or-columns]
  (map reverse rows-or-columns))

(defn move-cells [arr]
  (let [non-zero (filter pos? arr)]
    (let [add-similar (add-pairs non-zero)]
      (concat add-similar (repeat (- width (count add-similar)) 0)))))

(defn add-pairs [arr]
  (loop [r arr
         output []]
    (if (empty? r) output
        (let [cur (first r)
              next (second r)]
          (if (= cur next)
            (recur (drop 2 r) (conj output (* 2 cur)))
            (recur (drop 1 r) (conj output cur)))))))

(defn move-left [game-board]
  (mapcat move-cells 
          (get-rows game-board))
  )

(defn move-right [game-board]
  (mapcat reverse 
          (map move-cells 
               (map reverse 
                    (get-rows game-board))))
  )

(defn move-up [game-board]
  (flatten (get-columns 
            (mapcat move-cells 
                    (get-columns game-board))))
)

(defn move-down [game-board]
  (flatten (get-columns 
            (mapcat reverse 
                    (map move-cells 
                         (map reverse 
                              (get-columns game-board))))))
  )



(defn play-next [game-board]
  (println "Press \n w : move up \n a : move left \n s : move down \n d : move right ")
  (let [input (read-line)]
    (if (contains? #{"w" "a" "s" "d" "q"} input) 
      (execute input game-board))))


(defn executecase [input game-board]
  (case input
    "w" (move-up game-board)
    "a" (move-left game-board)
    "s" (move-down game-board)
    "d" (move-right game-board) 
    nil
  ))

(defn execute [input game-board]
  (cond
    (= "w" input) (move-up game-board)
    (= "a" input) (move-left game-board)
    (= "s" input) (move-down game-board)
    (= "d" input) (move-right game-board)
    (= "q" input) (println "quit")
    ))

(loop [game-board (play-next game-board)]
  (if (= game-board nil) (println "quit")
      (do
      (draw-board game-board)
      (recur (play-next game-board)))
  ))

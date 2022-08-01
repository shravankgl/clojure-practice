(ns clojure-2408.game)

(defn init-game [width win-num]
  "Initialize the game"
  (def width width)
  (def total-cells (* width width))
  (def win-num win-num)
  (def game-board
    (let [zero-board (vec (repeat (* total-cells) 0))]
      (println zero-board)
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

(defn draw-board [game-board width]
  "draw the board"
  (doseq [row  (get-rows game-board)] (println row)))


(defn get-rows [game-board]
  (partition-all 4 game-board))

(defn get-columns [game-board width]
  (list
   (vec (take-nth 4 game-board))
   (vec (take-nth 4 (drop 1 game-board)))
   (vec (take-nth 4 (drop 2 game-board)))
   (vec (take-nth 4 (drop 3 game-board)))))


(defn reverse-rows-or-columns [rows-or-columns]
  (map reverse rows-or-columns)
  )

(defn move-cells [arr width] 
  (let [non-zero (filter pos? arr)]
  (let [add-similar (reduce (fn [a b] (if (= a b) [(+ a b) 0] [a b])) non-zero)]
 
    (concat non-zero (repeat (- width (count non-zero)) 0))
    ))
  )

(init-game 4 2408)

(draw-board game-board width)

game-board

(get-columns game-board width)

(reverse-rows-or-columns (get-rows game-board))



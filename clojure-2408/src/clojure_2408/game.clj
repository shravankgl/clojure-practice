(ns clojure-2408.game)

(defn get-zero-indicies
  "get list of all indicies containing value 0"
  [game-board]
  (map first
       (filter #(= (second %) 0)
               (map-indexed vector game-board))))

(defn pick-random-empty-cell
  "get a random 0 value cell"
  [game-board]
  (if (= (apply min game-board) 0)
  (rand-nth (get-zero-indicies game-board))
    -1))

(defn pick-rand-value
  "pick a value between 2(75% probability) and 4(25% probability) "
  []
  (rand-nth [2 2 2 4]))

(defn add-cell
  "add a random value in random 0 value cell"
  [game-board]
  (let [random-cell (pick-random-empty-cell game-board)]
    (if (= random-cell -1) game-board
    (assoc (vec game-board) random-cell (pick-rand-value)))))

(defn get-rows [game-board]
  (let [width (int (Math/sqrt (count game-board)))]
    (partition-all width game-board)))

(defn get-columns [game-board]
   (let [width (int (Math/sqrt (count game-board)))] 
      (vec (apply map vector (partition width game-board))))
     )

(defn draw-board
  "draw the board"
  [game-board]
  (println "")
  (doseq [row  (get-rows game-board)] (println row))
  game-board)

(defn add-pairs [arr]
  (loop [r arr
         output []]
    (if (empty? r) output
        (let [cur (first r)
              next (second r)]
          (if (= cur next)
            (recur (drop 2 r) (conj output (* 2 cur)))
            (recur (drop 1 r) (conj output cur)))))))


(defn move-cells [arr]
  (let [non-zero (filter pos? arr) width (count arr)]
    (let [add-similar (add-pairs non-zero)]
      (concat add-similar (repeat (- width (count add-similar)) 0)))))

(defn move-left [game-board]
  (mapcat move-cells
          (get-rows game-board)))

(defn move-right [game-board]
  (mapcat reverse
          (map move-cells
               (map reverse
                    (get-rows game-board)))))

(defn move-up [game-board]
  (flatten (get-columns
            (mapcat move-cells
                    (get-columns game-board)))))

(defn move-down [game-board]
  (flatten (get-columns
            (mapcat reverse
                    (map move-cells
                         (map reverse
                              (get-columns game-board)))))))

(defn execute [input game-board]
  (cond
    (= "w" input) (move-up game-board)
    (= "a" input) (move-left game-board)
    (= "s" input) (move-down game-board)
    (= "d" input) (move-right game-board)))

(defn play-next [game-board]
  (let [input (read-line)]
    (if (contains? #{"w" "a" "s" "d" "q"} input)
      (if (= input "q") input
          (draw-board (add-cell (execute input game-board))))
      (do (println "invalid input") game-board))))

(defn no-same-adjacent [coll]
  (= coll (dedupe coll)))

(defn game-won? [game-board win-num]
  (= (apply max game-board) win-num))

(defn game-lost? [game-board]
  (and (every? no-same-adjacent (get-rows game-board))
       (every? no-same-adjacent (get-columns game-board))
       (> (apply min game-board) 0)))

(defn play-2048
  "Play 2048 the game"
  [width win-num]
  (println "Press \n w : move up \n a : move left \n s : move down \n d : move right \n q : quit")
  (let [total-cells (* width width)
        game-board (add-cell (vec (repeat (* total-cells) 0)))]
    (draw-board game-board)
    (loop [input game-board]
      (if (= input "q") (println "game over")
          (cond
            (game-won? input win-num)  (println "you win")
            (game-lost? input)  (println "you lose")
            :else (recur (play-next input)))))))


(ns clojure-2408.core
  (:gen-class) 

  (:require [clojure-2408.game :refer [play-2048]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (play-2048 4 2048)
  (println "Thank you for playing"))

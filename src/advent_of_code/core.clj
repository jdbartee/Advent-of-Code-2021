(ns advent-of-code.core
  (:gen-class) 
  (:require [advent-of-code.day-three :as today]))

(defn -main
  [& args]
  (today/run args))
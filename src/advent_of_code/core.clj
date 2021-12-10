(ns advent-of-code.core
  (:gen-class) 
  (:require [advent-of-code.day-four :as today]))

(defn -main
  [& args]
  (today/run args))
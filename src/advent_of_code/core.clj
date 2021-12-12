(ns advent-of-code.core
  (:gen-class) 
  (:require [advent-of-code.day-ten :as today]))

(defn -main
  [& args]
  (today/run args))
(ns advent-of-code.core
  (:gen-class) 
  (:require [advent-of-code.day-nine :as today]))

(defn -main
  [& args]
  (today/run args))
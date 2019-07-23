(ns euler
  (:require [clojure.math.numeric-tower :as math]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn bigrange [from to]
  (as-> (range from to) thing
        (mapcat (fn [x]
               (map (fn [u]
                      [x u])
                    thing))
             thing)))

(def problem29 (as-> (bigrange 2M 101M) thing
      (pmap (fn [[x y]]
              (.pow x y))
            thing)
      (into #{} thing)
      (count thing)))

(defn digits [n]
  (if (= n 0)
    []
    (conj (digits (quot n 10)) (rem n 10) )))

(def problem30
  (reduce +
          (dbg (filter
            (fn [n]
              (->> n
                (digits)
                (map (fn [n]
                       (math/expt n 5)))
                (apply +)
                (= n)))
            (range 2 1000000)))))

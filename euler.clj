(ns euler
  (:require [clojure.math.numeric-tower :as math]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn bigrange [s]
  (as-> s thing
        (mapcat (fn [x]
               (map (fn [u]
                      [x u])
                    thing))
             thing)))

(def problem29 (as-> (bigrange (range 2M 101M)) thing
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

(defn break [c v]
  (let [useful-coins (filter #(<= % v) c)]
    (if (empty? useful-coins)
      #{}
      (let [largest (first useful-coins)
            others (rest useful-coins)
            q (quot v largest)
            r (rem v largest)
            x (range q -1 -1)
            soln-seed
            (if (and (> q 0) (= r 0))
              #{{largest q}}
              #{})]
        (into soln-seed
              (mapcat
                (fn [l-num]
                  (let [solns-without-largest
                        (->> l-num
                          (* largest)
                          (- v)
                          (break others))]
                    (if (> l-num 0)
                      (map (fn [soln]
                             (assoc soln largest l-num))
                           solns-without-largest)
                      solns-without-largest)))
                x))))))

(defn test-break
  []
  (let [test-cases {
                    :c [25 10 5 1]
                    :vs [0 1 2 5 10 12 25]
                    :results
                    [#{}
                     #{{1 1}}
                     #{{1 2}}
                     #{{5 1} {1 5}}
                     #{{10 1} {5 2} {5 1 1 5} {1 10}}
                     #{{10 1 1 2} {5 2 1 2} {5 1 1 7} {1 12}}
                     #{{25 1} {10 2 5 1} {10 1 5 3} {10 1 5 2 1 5} {10 1 5 1 1 10} {10 1 1 15} {5 5} {5 4 1 5} {5 3 1 10} {5 2 1 15} {5 1 1 20} {1 25}}]
                    }]
    (doseq [baby (map (fn [x y] [x y]) (:results test-cases) (:vs test-cases))]
      (let [[expected given] baby
            received (break (:c test-cases) given)]
        (when (not (= received expected))
          (println "given: " given)
          (println "expected: " expected)
          (println "received: " received)
          (println "diff: " (diff expected received))
          )))))


         [

    })

   {
    :c [25 10 5 1]
    :v 17
    }

(defn verify-break [c v]
  (->> (break c v)
    (map
      (fn [soln]
        (= (reduce
             (fn [c [k v]]
               (+ c (* k v)))
             0
             soln)
           v)))
    (reduce
      #(and %1 %2)
      true)))

(def problem31
  (let [english-coins [1 2 4 5 10 20 50 100 200]]
    (count (break english-coins 200))))

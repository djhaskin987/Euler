(ns euler
  (:require [clojure.math.numeric-tower :as math]
            [clojure.data :as data]
            [util]))

(def problem29 (as-> (util/bigrange (range 2M 101M)) thing
      (pmap (fn [[x y]]
              (.pow x y))
            thing)
      (into #{} thing)
      (count thing)))

(def problem30
  (reduce +
          (util/dbg (filter
            (fn [n]
              (->> n
                (util/digits)
                (map (fn [n]
                       (math/expt n 5)))
                (apply +)
                (= n)))
            (range 2 1000000)))))

(println "30")
(println problem30)

(defn all-breaks [coins amount seed-soln]
  (if (zero? amount)
    (if (empty? seed-soln)
      #{}
      #{seed-soln})
    (let [effective-coins
          (filter #(<= % amount) coins)]
      (if (empty? effective-coins)
        #{(assoc seed-soln 1 amount)}
        (let [largest (first coins)
              others (rest coins)
              max-used (quot amount largest)]
          (into #{}
                (mapcat
            (fn [l]
              (let [d (- amount (* largest l))]
                (all-breaks others d (if (not (zero? l))
                                       (assoc seed-soln largest l)
                                       seed-soln))))
            (range max-used
                   (if (= largest 1)
                     0
                     -1)
                   -1))))))))

(defn verify-break [solns amount]
  (->> solns
    (map
      (fn [soln]
        (= (reduce
             (fn [c [k v]]
               (+ c (* k v)))
             0
             soln)
           amount)))
    (reduce
      #(and %1 %2)
      true)))

(defn test-break
  []
  (let [test-cases {
                    :c [25 10 5]
                    :vs [0 1 2 5 10 12 25]
                    :results
                    [#{}
                     #{{1 1}}
                     #{{1 2}}
                     #{{5 1} {1 5}}
                     #{{10 1} {5 2} {5 1 1 5} {1 10}}
                     #{{10 1 1 2} {5 2 1 2} {5 1 1 7} {1 12}}
                     #{{25 1} {10 2 5 1} {10 2 1 5} {10 1 5 3} {10 1 5 2 1 5} {10 1 5 1 1 10} {10 1 1 15} {5 5} {5 4 1 5} {5 3 1 10} {5 2 1 15} {5 1 1 20} {1 25}}]
                    }]
    (doseq [baby (map (fn [x y] [x y]) (:results test-cases) (:vs test-cases))]
      (let [[expected given] baby
            received (all-breaks (:c test-cases) given {})]
        (when (not (= received expected))
          (println "given: " given)
          (println "expected: " expected)
          (println "received: " received)
          (println "expected size: " (count expected))
          (println "received size: " (count received))
          (println "sorta: " (verify-break received given)
          (println "diff: " (data/diff expected received))
          ))))))

(def problem31
  (let [english-coins [200 100 50 20 10 5 2]]
    (all-breaks english-coins 200 {})))

(println "")
(println "31")
(println "")
(println (count problem31))

(def problem32
  (reduce
    (fn [c [x y]]
      (let [dx (util/digits x)
            dy (util/digits y)
            cdx (count dx)
            cdy (count dy)
            product-least-size (- (+ cdx cdy) 1)]
        (if (> (+ cdx cdy product-least-size) 9)
          c
          (let [xy (* x y)
                dxy (util/digits xy)
                cdxy (count dxy)]
            (if (and (= 9 (+ cdx
                             cdy
                             cdxy))
                     (=
                       [1 2 3 4 5 6 7 8 9]
                       (sort (reduce into []
                               [dx
                                dy
                                dxy]))))
          (conj c xy)
          c)))))
    #{}
    (util/bigrange (range 1 10000))))

(println "32")
(println "")
(println (apply + problem32))



(println "33")
(println "")


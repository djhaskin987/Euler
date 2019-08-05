(ns util)

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

(defn digits [n]
  (if (= n 0)
    []
    (conj (digits (quot n 10)) (rem n 10) )))


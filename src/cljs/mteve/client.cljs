(ns mteve.core
  (:use [c2.core :only [unify]]
        [c2.dom :only [replace!]])
  (:require [c2.scale :as scale]
            [vomnibus.color-brewer :as cb]))

(def parameters
  {:lambda 2.5
   :initial-population 8
   :max-gens 100})

(defn poisson [lambda]
  ;; Knuth gave us this tiny gem
  (let [e (.exp js/Math 1)
        L (.pow js/Math e (- lambda))]
    (loop [k 0 p 1]
      (if (> p L)
        (recur (+ k 1) (* p (rand)))
        (- k 1)))))

(defn take-while-with-last
  "A variation of take-while that preserves a last 'failed' element"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) (take-while-with-last pred (rest s)))
       (cons (first s) nil)))))

(defn simulate [lambda n max-gens]
  (defn next-gen [gen]
    (let [all-offsprings (mapcat #(repeat (poisson lambda) %) gen)
          survivors (sort (take n (shuffle all-offsprings)))]
      (vec survivors)))
  (let [initial (vec (range 1 (+ n 1)))]
    (->> (iterate next-gen initial)
         (take-while-with-last (fn [gen]
                                 (let [x (first gen)]
                                   (not (every? #(= % x) gen)))))
         (take max-gens)
         (vec))))

(defn plot-fragment []
  (let [data (simulate (:lambda parameters)
                       (:initial-population parameters)
                       (:max-gens parameters))
        entity-fn (fn [entity]
                    [:div.entity
                     {:style {:background-color (nth cb/Set1-8 (- entity 1))}}
                     entity])
        gen-fn (fn [gen]
                 (into [] (cons :div.gen (map entity-fn gen))))
        dom [:section#main (unify data gen-fn)]]
    (replace! "#main" dom)))

(plot-fragment)

(.addEventListener (.getElementById js/document "regen") "click"
                   (fn [evt] (.preventDefault evt) (plot-fragment)))

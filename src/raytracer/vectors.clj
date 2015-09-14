(ns raytracer.vectors
  (:use clojure.math.numeric-tower))

(def EPSILON 0.0001)

(def origin {:x 0, :y 0, :z 0})
(def xvector { :x 1, :y 0, :z 0})
(def yvector { :x 0, :y 1, :z 0})
(def zvector { :x 0, :y 0, :z 1})

(defn distance [x1 y1 x2 y2]
  (sqrt (+
          (expt  (- x2 x1) 2)
          (expt  (- y2 y1) 2))))

(defn magnitude [vector]
  (sqrt (+
          (expt (:x vector) 2)
          (expt (:y vector) 2)
          (expt (:z vector) 2))))

(defn normalize [vector]
  {:x (/ (:x vector) (magnitude vector)),
   :y (/ (:y vector) (magnitude vector)),
   :z (/ (:z vector) (magnitude vector))})

(defn negative [vector]
  {:x (- (:x vector)),
   :y (- (:y vector)),
   :z (- (:z vector))})

(defn dot [v1 v2]
  (+ (* (:x v1) (:x v2))
     (* (:y v1) (:y v2))
     (* (:z v1) (:z v2))))

(defn cross [v1 v2]
  {:x (- (* (:y v1) (:z v2)) (* (:z v1) (:y v1))),
   :y (- (* (:z v1) (:x v2)) (* (:x v1) (:z v2))),
   :z (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2)))})

(defn add-vector [v1 v2]
  {:x (+ (:x v1) (:x v2)),
   :y (+ (:y v1) (:y v2)),
   :z (+ (:z v1) (:z v2))})

(defn subtract-vector [v1 v2]
  {:x (- (:x v1) (:x v2)),
   :y (- (:y v1) (:y v2)),
   :z (- (:z v1) (:z v2))})

(defn multiply-vector [vector scalar]
  {:x (* (:x vector) scalar),
   :y (* (:y vector) scalar),
   :z (* (:z vector) scalar)})

(defn get-reflection-vector [vector normal]
  (add-vector (multiply-vector normal (* (dot vector normal) 2)) vector))

(defn get-vector-towards [starting-point towards]
  {:location starting-point :direction (normalize (subtract-vector towards starting-point))})

(defn get-intersection-point [ray distance]
  (let [rayorigin (:location ray)
        raydirection (:direction ray)]
    (add-vector rayorigin (multiply-vector raydirection distance))))
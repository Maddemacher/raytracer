(ns raytracer.objecthandling
  (:use mikera.image.colours)
  (:use mikera.image.core)
  (:use mikera.image.filters)
  (:use mikera.image.spectrum)
  (:use clojure.math.numeric-tower)
  (:use raytracer.vectors))

(defn get-sphere-normal [point sphere]
  (normalize (add-vector point (negative (:location sphere)))))

(defn get-normal-at [point object]
  (case (:type object)
    :sphere (get-sphere-normal point object)
    :plane (:normal object)))

(defn get-object-hit [object ray distance]
  (let [intersectionpoint (get-intersection-point ray distance)]
    {:object object, :normal (get-normal-at intersectionpoint object) :intersectionpoint intersectionpoint :direction (:direction ray) :distance distance}))

(defn get-object-miss [object]
  {:object object, :distance -1})

(defn find-plane-intersection [ray plane]
  (let [a (dot (:direction ray) (:normal plane))]
    (if (= a 0)
      (get-object-miss plane)
      (let [r (- (/ (* -1 (dot (:normal plane) (add-vector (:location ray) (negative (multiply-vector (:normal plane) (:distance plane)))))) a) EPSILON)]
        (get-object-hit plane ray r)))))

(defn find-sphere-intersection [ray sphere]
  (let [rayorigin (:location ray)
        rayoriginx (:x rayorigin)
        rayoriginy (:y rayorigin)
        rayoriginz (:z rayorigin)
        raydir (:direction ray)
        dirx (:x raydir)
        diry (:y raydir)
        dirz (:z raydir)
        spherecenter (:location sphere)
        spherecenterx (:x spherecenter)
        spherecentery (:y spherecenter)
        spherecenterz (:z spherecenter)
        a 1
        b (* 2 (+ (* dirx (- rayoriginx spherecenterx)) (* diry (- rayoriginy spherecentery)) (* dirz (- rayoriginz spherecenterz))))
        c  (- (+ (expt (- rayoriginx spherecenterx) 2) (expt (- rayoriginy spherecentery) 2) (expt (- rayoriginz spherecenterz) 2)) (expt (:r sphere) 2))
        discriminant (- (expt b 2) (* 4 a c))]
    (if (> discriminant 0)
      (let [r1 (- (/ (- (- b) (sqrt discriminant)) 2) EPSILON)
            r2 (- (/ (+ (- b) (sqrt discriminant)) 2) EPSILON)]
        (if (> r1 0)
          (get-object-hit sphere ray r1)
          (get-object-hit sphere ray r2)))
      (get-object-miss sphere))))

(defn find-intersection [ray object]
  (case (:type object)
    :sphere (find-sphere-intersection ray object)
    :plane (find-plane-intersection ray object)))

(defn find-closest [intersections]
  (first (drop-while #(neg? (:distance %1)) (sort-by :distance intersections ))))

(defn get-hit-object [ray objs]
  (find-closest (map #(find-intersection ray %) objs)))
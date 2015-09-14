(ns raytracer.colorhandling
  (:use raytracer.colors)
  (:use raytracer.vectors)
  (:use raytracer.objecthandling)
  (:use clojure.math.numeric-tower))

(defn get-color-of-object [hitobject]
  (case (:type (:object hitobject))
    :sphere (:color (:object hitobject))
    :plane (:color (:object hitobject))))

(defn in-shadow? [light intersectionPoint objs]
  (let [lightPosition (:location light)
        distanceToLight (subtract-vector lightPosition intersectionPoint)
        distanceToLigthMagnitude (magnitude distanceToLight)
        shadowRay (get-vector-towards intersectionPoint lightPosition)
        hitobject (get-hit-object shadowRay objs)]
    (if (nil? hitobject)
      false
      (if (<= (:distance hitobject) distanceToLigthMagnitude)
        true
        false))))

(defn get-specular [hitobject lightray]
    (dot (normalize (get-reflection-vector (negative (:direction lightray)) (:normal hitobject))) (:direction lightray)))

(defn get-color-of-illuminated-point [basecolor ambientlight light lightray lightangle hitobject]
  (let [diffuselight (scale-color (multiply-color basecolor (:color light)) lightangle)
        specularlight (get=specular hitobject lightray)]
      (add-color ambientlight diffuselight specularlight )))

(defn get-color-for-light [light hitobject objs amblight]
  (let [basecolor (get-color-of-object hitobject)
        lightray (get-vector-towards (:intersectionpoint hitobject) (:location light))
        lightangle (dot (:normal hitobject) (:direction lightray))
        ambientlight (scale-color basecolor amblight)]
    (if (pos? lightangle)
      (if (in-shadow? light (:intersectionpoint hitobject) objs)
        ambientlight
        (get-color-of-illuminated-point basecolor ambientlight light lightray lightangle hitobject))
      ambientlight)))

(defn get-color-at [hitobject objs, lights, amblight]
  (reduce average-colour (map #(get-color-for-light % hitobject objs amblight) lights)))

(defn get-color-for-ray [ray objs lights amblight]
  (let [hitobject (get-hit-object ray objs)]
    (if (nil? hitobject)
      (get-rgb 0 0 0)
      (get-color-at hitobject objs lights amblight))))
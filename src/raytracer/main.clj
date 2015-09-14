(ns raytracer.main
  (:use mikera.image.colours)
  (:use mikera.image.core)
  (:use mikera.image.filters)
  (:use mikera.image.spectrum)
  (:use clojure.math.numeric-tower)
  (:use raytracer.vectors)
  (:use raytracer.objecthandling)
  (:use raytracer.colors)
  (:use raytracer.colorhandling)
  (:import (java.io ByteArrayOutputStream ByteArrayInputStream)
           (javax.imageio ImageIO)))

(def image (new-image 480 480))
(def pixels (get-pixels image))
(def aspectratio (/ (width image) (height image)))

(def lookat {:x 0, :y 0, :z 4})

(def camlocation {:x 0, :y 3, :z -4})

(def objects (list
               {:type :sphere, :r 1, :location {:x 2.1, :y 0, :z 4}, :color (long-colour 0xFFFF0000) :shine 0.3},
               {:type :sphere, :r 1, :location {:x 0, :y 0, :z 4}, :color (long-colour 0xFF00FF00) :shine 0.3},
               {:type :sphere, :r 1, :location {:x -2.1, :y 0, :z 4}, :color (long-colour 0xFF0000FF) :shine 0.3},
               {:type :plane, :normal {:x 0, :y 1, :z 0}, :distance -1, :color (long-colour 0xFFF0F0F0) :shine 0},
               ))

(def lightsources (list {:location {:x -3, :y 10, :z 0}, :color (get-rgb-float 1 1 1)},
                        {:location {:x 3, :y 10, :z 0}, :color (get-rgb-float 1 1 1)}))

(def ambientlight 0.2)

(defn get-camera [location lookat]
  (let [camdirection (normalize (negative (subtract-vector location lookat)))
        camright (cross yvector camdirection)
        camdown (negative (cross camright camdirection))]
    {:location location,
     :direction camdirection,
     :camright camright,
     :camdown camdown}))

(defn get-xy-amount [x y image]
  (let [h (height image)
        w (width image)]
  (if (> w h)
    {:xamnt (- (* (/ (+ x 0.5) w) aspectratio) (/ (/ (- w h) h) 2)),
     :yamnt (/ (+ (- h y) 0.5) h)}
    (if (> h w)
      {:xamnt (/ (+ x 0.5) w),
       :yamnt (/ (/ (+ (- h y) 0.5) h) aspectratio)}
      {:xamnt (/ (+ x 0.5) w),
       :yamnt (/ (+ (- h y) 0.5) h)}))))

(defn ray-trace [objs lights amblight camera image]
  (dotimes  [y (height image)]
    (dotimes [x (width image)]
      (let [xyamnt (get-xy-amount x y image)
            camRayDirection (normalize
                              (add-vector (:direction camera)
                                         (add-vector
                                           (multiply-vector (:camright camera) (- (:xamnt xyamnt) 0.5))
                                           (multiply-vector (:camdown camera) (- (:yamnt xyamnt) 0.5)))))
            ray {:location (:location camera), :direction camRayDirection}]
            (aset pixels (+ x (* y (width image))) (get-color-for-ray ray objs lights amblight)))))
  (set-pixels image pixels))

(defn get-stream-from-image [image]
  (let [out-stream (ByteArrayOutputStream.)
        in-stream (do
                    (ImageIO/write image "png" out-stream)
                    (ByteArrayInputStream.
                      (.toByteArray out-stream)))]
    in-stream))

(defn get-default-traced-image []
  (ray-trace objects lightsources ambientlight (get-camera camlocation lookat) image)
  (get-stream-from-image image))

(defn get-traced-image [params]
  (ray-trace (:objects params) (:lightsources params) (:ambientlight params) (get-camera (:cameralocation params) (:lookat params)) image)
  (get-stream-from-image image))

(defn show-traced-image [params]
  (ray-trace (:objects params) (:lightsources params) (:ambientlight params) (get-camera (:cameralocation params) (:lookat params)) image)
  (show image :zoom 1 :title "Isn't it beautiful?"))

(defn trace []
  (ray-trace objects lightsources ambientlight (get-camera camlocation lookat) image)
  (show image :zoom 1 :title "Isn't it beautiful?"))

(ns raytracer.colors
  (:use mikera.image.colours)
  (:use clojure.math.numeric-tower))

(defn hexify [x]
  (format "%x" x))

(defn get-channels [c]
  {:red (/ (extract-red c) 255)
   :green (/ (extract-green c) 255)
   :blue (/ (extract-blue c) 255)})

(defn add-chanel [hex value shiftamnt]
  (bit-or hex (bit-shift-left (long (min value 255)) shiftamnt)))

(defn add-red [hex red] (add-chanel hex red 16))

(defn add-green [hex green] (add-chanel hex green 8))

(defn add-blue [hex blue] (add-chanel hex blue 0))

(defn get-rgb [r g b]
  (long-colour  (add-blue (add-green (add-red 0xFF000000 r) g) b)))

(defn get-rgb-float [r g b]
  (get-rgb (* r 255) (* g 255) (* b 255)))

(defn get-from-hex-string [hexstring]
  (long-colour (read-string hexstring)))

(defn scale-color [colour scale]
  (let [channels (get-channels colour)
        red (* (:red channels) scale)
        green (* (:green channels) scale)
        blue (* (:blue channels) scale)]
    (get-rgb-float red green blue)))

(defn get-brightness [c]
  (let [channels (get-channels c)]
    (/ (+ (:red channels) (:green channels) (:blue channels)) 3)))

(defn modify-color-channels-with-function [f c1 c2]
  (let [c1channels (get-channels c1)
        c2channels (get-channels c2)]
    (get-rgb-float (f (:red c1channels) (:red c2channels)) (f (:green c1channels) (:green c2channels)) (f (:blue c1channels) (:blue c2channels)))))

(defn add-color [& args]
   (reduce #(modify-color-channels-with-function + %1 %2) args))

(defn multiply-color [c1 c2]
  (modify-color-channels-with-function * c1 c2))

(defn average-colour [c1 c2]
  (modify-color-channels-with-function #(/ (+ %1 %2) 2) c1 c2))
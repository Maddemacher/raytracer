(ns raytracer.queryhandler
  (:use raytracer.colors)
  (:require [clojure.data.json :as json]))

(defn trace-value-converter [key value]
  (case key
    :type (keyword value)
    :color (get-from-hex-string value)
    value))

(defn trim-query [requeststring]
  (clojure.string/replace (clojure.string/replace requeststring #"%22" "\"") #"%20" ""))

(defn convert-trace-request [requeststring]
  (json/read-str (trim-query requeststring) :key-fn keyword :value-fn trace-value-converter))
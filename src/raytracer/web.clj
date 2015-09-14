(ns raytracer.web
  (:use ring.middleware.json)
  (:use ring.adapter.jetty)
  (:use raytracer.rest))


(def app
  (-> handler
      wrap-json-params))

(defn run [] (run-jetty #'app {:port 8080}))

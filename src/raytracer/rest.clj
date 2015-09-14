(ns raytracer.rest
  (:use raytracer.main)
  (:use raytracer.queryhandler)
  (:use compojure.core)
  (:require [compojure.route :as route])
  (:require [clojure.data.json :as json]))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/write-str data)})

(defn png-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "image/png"}
   :body data})

(defroutes handler
           (GET "/" request (png-response (get-default-traced-image)))

           (GET "/Trace" request
             (println (str (:query-string request)))
             (let [query (convert-trace-request (str (:query-string request)))]
               (println query)
               (png-response (get-traced-image query))))

           (GET "/Query" request
             (json-response {"ARGS" (str (:query-string request))}))

           (GET "/Params" request
                (json-response {"ARGS" (str request)}))

           (PUT "/" [name]
                (json-response {"hello" name}))

           (route/not-found "<h1>You done fucked up now</j1>"))

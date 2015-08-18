(ns ttt.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [monger.core :as mg]
            [monger.collection :as mc]
            [cheshire.core :refer [generate-string, parse-string]]
            [ring.util.response :as response]
            [ring.middleware.json :as middleware]))

(cheshire.generate/add-encoder org.bson.types.ObjectId cheshire.generate/encode-str)

(defn generate
  "generates a unique token for the game"
  []
  (str (System/currentTimeMillis)))

(def hotseat 0)

(let [conn (mg/connect)
      db   (mg/get-db conn "ttt")
      coll "games"]

  (defn get-all-games
    "Returns a list of games available on the server"
    []
    (generate-string
      (mc/find-maps db coll)))

  (defn new-game
    "Creates a new game"
    [body]
    (let [type (get body "type")]
      (if
        (= type hotseat)
          (generate-string
             (mc/insert-and-return db coll {:token (generate) :type type}))
        "Bad request")))

  (defn get-game
    "Returns a status of a game by id"
    [id]
    (generate-string
        (mc/find-one db coll {:token id} [:token :state :field]) false)))

(defroutes app-routes
  (context "/games" [] (defroutes games-routes
     (GET "/" [] (get-all-games))
     (POST "/" {body :body} (new-game body))
     (context "/:id" [id] (defroutes game-routes
        (GET "/" [] (get-game id))))))

  (GET "/" [] (response/file-response "index.html" {:root "resources/public"}))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (-> (handler/api app-routes)
    (middleware/wrap-json-body)
    (middleware/wrap-json-response)))
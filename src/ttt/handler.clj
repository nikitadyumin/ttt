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

(def empty-field [0,0,0,0,0,0,0,0,0])

(defn generate
  "generates a unique token for the game"
  []
  (str (System/currentTimeMillis)))

(defn fail
  "returns a serialized json with error description"
  [error]
  (case error
    :wrong-turn (generate-string {:error "wrong turn"})
    :invalid-state (generate-string {:error "invalid state"})
    :not-found (generate-string {:error "resource not found"})
    (generate-string {:error "unknown error"})))

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
    (let [{type "type" password "password"} body]
      (case type
        0 (generate-string
                  (mc/insert-and-return db coll {:token (generate) :type type :field empty-field :state :first-player-turn}))
        1 "shared key"
        "asd Bad request")))

  (defn get-game
    "Returns a status of a game by id"
    [id]
    (generate-string
        (mc/find-one db coll {:token id} [:token :state :field]) false))

  (defn make-turn
    "Make a turn"
    [id, body]
    (let [game (mc/find-one-as-map db coll {:token id})
          {state :state field :field token :token} game
          {player "player" position "position"} body]
      (prn player)
      (case state
        "first-player-turn" (cond
                               (= player 1) (generate-string (mc/save-and-return db coll (assoc game :state :second-player-turn)))
                               :else (fail :wrong-turn))
        "second-player-turn" (cond
                               (= player 2) (generate-string (mc/save-and-return db coll (assoc game :state :first-player-turn)))
                               :else (fail :wrong-turn))
        "first-player-won" (generate-string game)
        "second-player-won" (generate-string game)
        "tie" (generate-string game)
        (fail :invalid-state)))))

(defroutes app-routes
  (context "/games" [] (defroutes games-routes
     (GET "/" [] (get-all-games))
     (POST "/" {body :body} (new-game body))
     (context "/:id" [id] (defroutes game-routes
        (GET "/" [] (get-game id))
        (PUT "/" {body :body} (make-turn id body))))))

  (GET "/" [] (response/file-response "index.html" {:root "resources/public"}))
  (route/resources "/")
  (route/not-found (fail :not-found)))

(def app
  (-> (handler/api app-routes)
    (middleware/wrap-json-body)
    (middleware/wrap-json-response)))
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
  (let [acc [(str (System/currentTimeMillis))]]
    (loop [x 10 acc acc]
      (if (= x 0)
        (clojure.string/join "" acc)
        (recur (- x 2) (cons (str (rand-int 10000)) acc))))))

(defn fail
  "returns a serialized json with error description"
  [error]
  (case error
    :wrong-turn {:status 400 :body (generate-string {:error "wrong turn"})}
    :occupied-cell {:status 400 :body (generate-string {:error "the cell is already occupied"})}
    :invalid-state {:status 400 :body (generate-string {:error "invalid state"})}
    :not-found {:status 404 :body (generate-string {:error "resource not found"})}
    :not-implemented {:status 501 :body (generate-string {:error "not implemented"})}
    :bad-request  {:status 400 :body (generate-string {:error "bad request"})}
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
    (let [empty-field 0
          {type "type" password "password"} body]
      (case type
        0 {:status 201
           :body (generate-string
                   (mc/insert-and-return db coll {
                                                   :token (generate)
                                                   :type type
                                                   :field1 empty-field
                                                   :field2 empty-field
                                                   :state :first-player-turn}))}
        1 (fail :not-implemented)
        (fail :bad-request))))

  (defn get-game
    "Returns a status of a game by id"
    [id]
    (generate-string
        (mc/find-one db coll {:token id} [:token :state :field1 :field2]) false))

  (defn win? [field]
    "determines if the field has a winning situation"
    (cond
      (= (bit-and field 7) 7) true
      (= (bit-and field 56) 56) true
      (= (bit-and field 448) 448) true
      (= (bit-and field 292) 292) true
      (= (bit-and field 146) 146) true
      (= (bit-and field 73) 73) true
      (= (bit-and field 273) 273) true
      (= (bit-and field 84) 84) true
      :else false))

  (defn tie? [field1 field2]
    "checks if the board is full"
    (= (bit-or field1 field2) 511))

  (defn get-state [current-state field1 field2]
    "given the current state and a board determines state transition"
    (cond
      (win? field1) :first-player-wins
      (win? field2) :second-player-wins
      (tie? field1 field2) :tie
      (= current-state "first-player-turn") :second-player-turn
      (= current-state "second-player-turn") :first-player-turn
      :else (fail :invalid-state)))

  (defn add-and-check [position game]
    "makes a move if the target cell is not occupied"
    (let [field1 (get game :field1)
          field2 (get game :field2)
          state (get game :state)
          pos-mask (bit-shift-left 1 position)]
      (cond
        (= (bit-and (bit-or field1 field2) pos-mask) 0)
        (case state
          "first-player-turn" (generate-string
                                (mc/save-and-return db coll
                                  (assoc game
                                    :state (get-state state (bit-or field1 pos-mask) field2)
                                    :field1 (bit-or field1 pos-mask))))
          "second-player-turn" (generate-string
                                 (mc/save-and-return db coll
                                   (assoc game
                                     :state (get-state state field1 (bit-or field2 pos-mask))
                                     :field2 (bit-or field2 pos-mask)))))
          :else (fail :occupied-cell))))

  (defn make-turn
    "Make a move"
    [id, body]
    (let [game (mc/find-one-as-map db coll {:token id})
          {state :state field1 :field1 field2 :field2 token :token} game
          {player "player" position "position"} body]
      (case state
        "first-player-turn" (cond
                               (= player 1) (add-and-check position game)
                               :else (fail :wrong-turn))
        "second-player-turn" (cond
                               (= player 2) (add-and-check position game)
                               :else (fail :wrong-turn))
        "first-player-wins" (generate-string game)
        "second-player-wins" (generate-string game)
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
  (route/not-found (fail :bad-request)))

(def app
  (-> (handler/api app-routes)
    (middleware/wrap-json-body)
    (middleware/wrap-json-response)))
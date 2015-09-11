module Game (main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (fps, Time)
import Signal exposing (..)

import Thing exposing (Keys, Point, dist)
import Rocket
import Asteroid
import Bullets

-- MODEL
type Game = Playing | Ended

type alias Model =
    { state: Game
    , rocket: Thing.Model
    , asteroids : List Asteroid.Model
    , bullets : Bullets.Model
    }


init =
    { state = Playing
    , rocket = Rocket.init
    , asteroids = [Asteroid.init 125 50 6 1.5 20]
    , bullets = { bullets = [], lastBullet = 0 }
    }

-- UPDATE

update : Input -> Model -> Model
update (Input t sp ks) model =
-- update : Keys -> Model -> Model
-- update ks model =
    if model.state == Playing
    then
        let
            movedThings = moveThings ks model
            newBullets =
                -- if <space> then add bullet
                -- filter bullets that hit an asteroid
                Bullets.filterHits movedThings.asteroids <|
                    if sp && (t - movedThings.bullets.lastBullet > 1000)
                    then Bullets.addBullet movedThings.rocket t movedThings.bullets
                    else movedThings.bullets

            newAsteroids =
                Asteroid.filterHits movedThings.bullets.bullets movedThings.asteroids

        in case List.any (Asteroid.hit movedThings.rocket) movedThings.asteroids of
            True -> { model | state <- Ended }
            False ->
                { movedThings |
                      bullets <- newBullets
                    , asteroids <- newAsteroids
                }
    else model

moveThings : Keys -> Model -> Model
moveThings ks model =
    { model |
      rocket <- Rocket.update ks model.rocket
    , asteroids <- List.map Asteroid.update model.asteroids
    , bullets <- Bullets.update model.bullets
    }

-- checkCrash : Model -> Bool
-- checkCrash model = False

-- VIEW
view : Model -> Element
view model =
    if model.state == Playing
    then
        collage 300 300 <|
            (rect 300 300
                |> filled black)
            :: Rocket.view model.rocket
            :: List.map Asteroid.view model.asteroids
            ++ Bullets.view model.bullets
    else
        collage 300 300 <|
            [rect 300 300
                |> filled black]

viewDebug : Model -> Input -> Element
viewDebug model (Input t sp p) =
    flow right <|
        view model
        -- :: List.map (\m -> show m.thing.pos.x) model.asteroids
        :: [show <| List.length model.bullets.bullets ]

-- INPUT
-- arrows, wasd     :: Signal { x :: Int, y :: Int }
-- shift,ctrl,space :: Signal Bool
type Input
    = Input Time Bool {x: Int, y:Int}

input : Signal Input
input =
    let
        delta = Time.every 100
    in
        Signal.map3 Input delta Keyboard.space Keyboard.arrows

-- MAIN
main : Signal Element
main =
    let
        -- gameplay = fps 10
        -- signals = Signal.sampleOn gameplay Keyboard.arrows
        -- model = foldp update init signals
        model = foldp update init input
    in Signal.map2 viewDebug model input
    -- in Signal.map view model

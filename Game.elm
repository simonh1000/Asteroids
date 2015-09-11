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
import Bullet

-- MODEL
type Game = Playing | Ended

type alias Model =
    { state: Game
    , rocket: Thing.Model
    , asteroids : List Asteroid.Model
    , bullets : List Bullet.Model
    }


init =
    { state = Playing
    , rocket = Rocket.init
    , asteroids = [Asteroid.init]
    , bullets = []
    }

-- UPDATE

update : Keys -> Model -> Model
update ks model =
    if model.state == Playing
    then
        let
            newModel = moveThings ks model
            -- filter shot Meteorites
        in case List.any (Asteroid.checkCrash newModel.rocket) newModel.asteroids of
            True -> { model | state <- Ended }
            False -> newModel
    else model

moveThings : Keys -> Model -> Model
moveThings ks model =
    { model |
      rocket <- Rocket.update ks model.rocket
    , asteroids <- List.map Asteroid.update model.asteroids
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
            ++ List.map Bullet.view model.bullets
    else
        collage 300 300 <|
            [rect 300 300
                |> filled black]

viewDebug : Model -> Element
viewDebug model =
    flow right <|
        view model
        -- :: List.map (\m -> show m.thing.pos.x) model.asteroids
        :: [show <| List.map (\a -> dist model.rocket.pos a.thing.pos) model.asteroids ]

-- INPUT
-- arrows, wasd     :: Signal { x :: Int, y :: Int }
-- shift,ctrl,space :: Signal Bool
type Input
    = Keys {x: Int, y:Int}
    | Space Time Bool

input : Signal Input
input =
    let
        delta = Signal.map (\t -> t/20) (fps 30)
    in
        Signal.sampleOn delta (Signal.mergeMany [Signal.map Keys Keyboard.arrows, Signal.map2 Space delta Keyboard.space])

-- MAIN
main : Signal Element
main =
    let
        gameplay = fps 10
        signals = Signal.sampleOn gameplay Keyboard.arrows
        model = foldp update init signals
    in Signal.map viewDebug model
    -- in Signal.map view model

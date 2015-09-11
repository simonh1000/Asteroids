module Game (main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (fps)
import Signal exposing (..)

import Thing exposing (Keys, Point)
import Rocket
import Asteroid

-- MODEL
type alias Model =
    { rocket: Thing.Model
    , asteroids : List Asteroid.Model
    -- , bullets : [Model]
    }


init =
    { rocket = Rocket.init
    , asteroids = [Asteroid.init]
    }

-- UPDATE

update : Keys -> Model -> Model
update ks model =
    { model |
      rocket <- Rocket.update ks model.rocket
    , asteroids <- List.map Asteroid.update model.asteroids
    }

-- VIEW
view : Model -> Element
view model =
    collage 300 300 <|
        (rect 300 300
            |> filled black)
        :: Rocket.view model.rocket
        :: List.map Asteroid.view model.asteroids

viewDebug : Model -> Element
viewDebug model =
    flow right <|
        view model
        :: List.map (\m -> show m.thing.pos.x) model.asteroids

-- MAIN
main : Signal Element
main =
    let
        gameplay = fps 10
        signals = Signal.sampleOn gameplay Keyboard.arrows
        model = foldp update init signals
    in Signal.map viewDebug model
    -- in Signal.map view model

module Bullets (init, view, update, addBullet, filterHits, Model) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug
import Thing exposing (Point, floatMod, dist)
import Time exposing (Time)

import Asteroid

-- MODEL
type alias Model =
    { bullets : List Thing.Model
    , lastBullet : Float
    }

init : Point -> Float -> Float -> Thing.Model
init pt d s =
    { pos = pt
    , direction = d
    , speed = s
    }

addBullet : Thing.Model -> Time -> Model -> Model
addBullet rocket t model =
    let newBullet = { rocket | speed <- rocket.speed + 1}
    in { model |
             bullets <- newBullet :: model.bullets
           , lastBullet <- t
       }

-- UPDATE
update : Model -> Model
update model =
    let
        t1 = List.map (\b -> { b | pos <- Thing.move b} ) model.bullets
        t2 = List.filter notlost t1      -- remove bullets moving off-screen
    in { model | bullets <- t2 }

-- shoot : Input -> Model -> Model
-- shoot inp model = model

notlost : Thing.Model -> Bool
notlost model =
    abs model.pos.x < 150 &&
    abs model.pos.y < 150

filterHits : List Asteroid.Model -> Model -> Model
filterHits asts model =
    let
        filteredBullets = List.filter (\b -> List.all (\a -> Asteroid.miss b a) asts) model.bullets
    in { model | bullets <- filteredBullets }

-- VIEW

view : Model -> List Form
view model = List.map viewBullet model.bullets

viewBullet : Thing.Model -> Form
viewBullet model =
    circle 2
        |> filled white
        |> move (model.pos.x, model.pos.y)

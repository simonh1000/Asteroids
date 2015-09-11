module Bullet (init, view, update, Model) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug
import Thing exposing (Point, floatMod, dist)

-- MODEL
type alias Model =
    { bullet : Thing.Model
    , lastBullet : Int
    }

init : Point -> Float -> Float -> Int -> Model
init pt d s t =
  { bullet =
        { pos = pt
        , direction = d
        , speed = s
        }
    , lastBullet = t
    }

-- UPDATE
update : Model -> Model
update model =
    let
        t1 = model.bullet
        t2 = { t1 | pos <- Thing.move t1 }
    in { model | bullet <- t2 }

-- shoot : Input -> Model -> Model
-- shoot inp model = model

lost : Model -> Bool
lost model =
    abs model.bullet.pos.x > 150 ||
    abs model.bullet.pos.y > 150

-- VIEW

view : Model -> Form
view model =
    circle 2
        |> filled white
        |> move (model.bullet.pos.x, model.bullet.pos.y)

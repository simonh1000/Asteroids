module Asteroid (init, view, update, Model) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug
import Thing exposing (Point, floatMod)

-- MODEL
type alias Model =
    { thing : Thing.Model
    , size : Float
    }

init : Model
init =  { thing =
            { pos = { x = 125, y = 50}
            , direction = 6.0
            , speed = 1.5
            }
        , size = 20
        }


-- UPDATE
update : Model -> Model
update model =
    -- { model | thing | pos <- Thing.move model }
    let
        t1 = model.thing
        -- Debug.watch "Direciton" (bounce model)
        t2 = { t1 | direction <- bounce model }
        t3 = { t2 | pos <- Thing.move t2 }
        -- newthing = { model.thing | pos <- Thing.move model.thing }
    in { model | thing <- t3 }

bounce : Model -> Float
bounce model =
    if | abs model.thing.pos.x > (150 - model.size) ->
            floatMod (pi - model.thing.direction) (2 * pi)
       | abs model.thing.pos.y > (150 - model.size) ->
            floatMod (2 * pi - model.thing.direction) (2 * pi)
       | otherwise -> model.thing.direction

-- VIEW

view : Model -> Form
view model =
    ngon 5 model.size
        |> filled white
        |> move (model.thing.pos.x, model.thing.pos.y)

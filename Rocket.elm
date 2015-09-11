module Rocket (init, update, view) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Thing exposing (Keys, Point, Model, keepIn)

-- MODEL : Uses Model from Thing

init : Model
init =  { pos = { x = 0, y = 0}
        , direction = 0.0
        , speed = 0.0
        }

-- UPDATE

update : Keys -> Model -> Model
update {x,y} model =
    { model |
        pos <- newPos model
    ,   direction <- model.direction - (toFloat x / 10)   -- calibrate as you wish
    ,   speed <- minmax <| model.speed + (toFloat y / 3)
    }

newPos : Model -> Point
newPos model =
    { x = keepIn (model.pos.x + model.speed * cos model.direction) -150 150
    , y = keepIn (model.pos.y + model.speed * sin model.direction) -150 150
    }

minmax x = max 0 <| min x 5

-- VIEW

view : Model -> Form
view model =
    polygon [(10,0),(-8,-5),(-4,0),(-8,5)]     -- : Shape
        |> filled white
        |> rotate model.direction
        |> move (model.pos.x, model.pos.y)

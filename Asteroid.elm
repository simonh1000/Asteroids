module Asteroid (init, view, update, hit, miss, filterHits, Model) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug
import Thing exposing (Point, floatMod, dist)

-- MODEL
type alias Model =
    { thing : Thing.Model
    , size : Float
    }

init : Float -> Float -> Float -> Float -> Float -> Model
init xs ys d s sz =
    { thing =
        { pos = { x = xs, y = ys}
        , direction = d
        , speed = s
        }
    , size = sz
    }

-- UPDATE
update : Model -> Model
update model =
    let
        t1 = model.thing
        -- Debug.watch "Direciton" (bounce model)
        t2 = { t1 | direction <- bounce model }
        t3 = { t2 | pos <- Thing.move t2 }
        -- newthing = { model.thing | pos <- Thing.move model.thing }
    in { model | thing <- t3 }
    -- { model |
    --     thing <-
    --         { model.thing |
    --             direction <- bounce model
    --           , pos <- Thing.move model.thing
    --         }
    --     }

bounce : Model -> Float
bounce model =
    if | abs model.thing.pos.x > (150 - model.size) ->
            floatMod (pi - model.thing.direction) (2 * pi)
       | abs model.thing.pos.y > (150 - model.size) ->
            floatMod (2 * pi - model.thing.direction) (2 * pi)
       | otherwise -> model.thing.direction

-- concatMap : (a -> List b) -> List a -> List b
filterHits : List Thing.Model -> List Model -> List Model
filterHits bullets asteroids =
    -- List.filter (missAll bullets) asteroids
    List.concatMap (smash bullets) asteroids

smash : List Thing.Model -> Model -> List Model
smash bullets a =
    if missAll bullets a
        then [a]
        else if a.size > 10
            then
                [ init a.thing.pos.x a.thing.pos.y (a.thing.direction + pi /2) a.thing.speed (a.size / 2)
                , init a.thing.pos.x a.thing.pos.y (a.thing.direction - pi /2) a.thing.speed (a.size / 2) ]
            else []

-- VIEW

view : Model -> Form
view model =
    ngon 5 model.size
        |> filled white
        |> move (model.thing.pos.x, model.thing.pos.y)

-- HELPERS
hit : Thing.Model -> Model -> Bool
hit thingModel astModel = dist thingModel.pos astModel.thing.pos < astModel.size ^ 2

miss : Thing.Model -> Model -> Bool
miss t a = not <| hit t a

-- all Bullets miss this Asteroid
missAll : List Thing.Model -> Model -> Bool
missAll things model = List.all (\t -> miss t model) things

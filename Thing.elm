module Thing ( (#=), Point, Model, Keys, move, floatMod, keepIn, dist) where

(#=) : Point -> Point -> Bool
(#=) p1 p2 = p1.x==p2.x && p1.y==p2.y

infixr 4 #=

-- MODEL
type alias Point =
    { x : Float
    , y : Float
    }

type alias Model =
    { pos       : Point
    , direction : Float
    , speed     : Float
    }

type alias Keys =
    { x : Int
    , y : Int
    }

-- UPDATE

move : Model -> Point
move model =
    { x = model.pos.x + model.speed * cos model.direction
    , y = model.pos.y + model.speed * sin model.direction
    }

-- HELPERS
-- posToInt : Point -> (Int, Int)
-- posToInt {x,y} = (round x, round y)

floatMod : Float -> Float -> Float
floatMod n m = keepIn n 0 m

keepIn : Float -> Float -> Float -> Float
keepIn n mi ma =
    let dist = ma - mi
    in if | n < mi  -> keepIn (n + dist) mi ma
          | n >= ma -> keepIn (n - dist) mi ma
          | otherwise -> n

dist : Point -> Point -> Float
dist p1 p2 = (p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2

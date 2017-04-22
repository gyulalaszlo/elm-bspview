module Bsp.Css exposing (..)
{-| Describe me please...
-}

import Color
import Color.Convert
import Dict exposing (Dict)
import Regex


templateWith : Dict String String -> String -> String
templateWith params =
    Regex.replace Regex.All (Regex.regex <| "\\{\\{\\s*([^\\s{}]*)\\s*\\}\\}")
        (\{submatches, match} ->
            Maybe.withDefault match <|
            case submatches of
                [Just key] -> Dict.get key params
                _ ->
                    Just "#133700"
--                    Nothing
                    )


css : List (String, String) -> String -> String
css params ss = templateWith (Dict.fromList params) ss


{-| A single dimension size (like 1px or 50% or 0.2em).
-}
type CssDimension
    = Pixel Float
    | Percent Float
    | Em Float



dim : CssDimension -> String
dim d =
    case d of
        Pixel px -> toString px ++ "px"
        Percent pc -> toString pc ++ "%"
        Em pc -> toString pc ++ "em"


negate : CssDimension -> CssDimension
negate d =
    case d of
        Pixel v -> Pixel -v
        Percent v -> Percent -v
        Em v -> Em -v



styleAttr k x vs = (k, x) :: vs
dimAttr k x vs = (k, dim x) :: vs
left = dimAttr "left"
right = dimAttr "right"
top = dimAttr "top"
bottom = dimAttr "bottom"
width = dimAttr "width"
height = dimAttr "height"

p0 = Percent 0
p100 = Percent 0

positionAbsolute = styleAttr "position" "absolute"


color = Color.Convert.colorToCssRgba


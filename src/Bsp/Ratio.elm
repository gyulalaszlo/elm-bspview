module Bsp.Ratio exposing (..)
{-| Describe me please...
-}

{-| The split ratio for a BSP is pretty simple: either one size is
fixed to some value or both are equal.
-}

type Ratio
    = FixedA Float
    | FixedB Float
    | Equal


flipRatio : Ratio -> Ratio
flipRatio r =
    case r of
        FixedA d ->
            FixedB d

        FixedB d ->
            FixedA d

        _ ->
            r



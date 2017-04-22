module Bsp.Msg exposing (..)

{-| Describe me please...
-}

-- MSG -------------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor)
import Bsp.Ratio exposing (Ratio)
import Bsp.SplitView exposing (Direction)


type alias Id =
    Int


type Msg msg local
    = ChildMsg Id msg
    | SharedMsg msg

    -- BSP commands
    | Select Cursor
    | DeleteAt Cursor
    | SplitAt Cursor Direction local
    | SetLayoutEditingMode LayoutEditingMode
    | SetDirection Direction Cursor
    | SwapLR Cursor
    | Rotate Bsp.SplitView.RotateDirection Cursor
    | RotateParent Bsp.SplitView.RotateDirection Cursor
    | ResizeAt Ratio Cursor



-- LAYOUT EDITING MODES --------------------------------------------------------


{-| What kind of layout editing mode are we in
-}
type LayoutEditingMode
    = NotEditingLayout
    | EditingLayoutBlocks



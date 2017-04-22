module Colors.Monokai exposing (..)
{-| Describe me please...
-}

import Color exposing (Color, rgb)


darkBlack : Color
darkBlack = rgb 0x13 0x14 0x11

black : Color
black = rgb 0x27 0x28 0x22

white : Color
white = rgb 0xf8 0xf8 0xf2

pink : Color
pink = rgb 0xf9 0x26 0x72

cyan : Color
cyan = rgb 0x66 0xd9 0xef

green : Color
green = rgb 0xa6 0xe2 0x2e

orange : Color
orange = rgb 0xfd 0x97 0x1f



vimbg = rgb 0x1b 0x1d 0x1e
vimfg = rgb 0xf8 0xf8 0xf2

theme : ColorTheme
theme =
    { background = black
    , text = white

    , selection = orange
    , highlight = cyan
    }


---

{-| A generic color theme with basic colors
-}
type alias ColorTheme =
    { background: Color
    , text: Color

    , selection: Color
    , highlight: Color


    }

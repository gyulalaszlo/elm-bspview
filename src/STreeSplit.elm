module STreeSplit exposing (..)
{-| Describe me please...
-}
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Task



import STree exposing (STree)
import Units.Compose.N2 as N2 exposing (N2)
import Units.Compose.Range as Range exposing (Range)
import Units.Compose.Rect as Rect exposing (Rect)
import Window


--


unitRect : Rect number
unitRect = Rect.from 0 1 0 1

toCssSides : Rect v -> (v,v,v,v)
toCssSides {left, right, top, bottom} = (top,right,bottom,left)

rectToPixels : (number -> v -> number) -> Rect number -> Rect v -> Rect number
rectToPixels toPixelValue parentInPixels frame =
    Rect.map2 toPixelValue parentInPixels frame



{-|
-}
type alias LeafWrapper t =
    { leaf : t
    , size: Float
    }

wrapLeaf : t -> LeafWrapper t
wrapLeaf t =
    { leaf = t
    , size = 1.0
    }


leaf : LeafWrapper t -> t
leaf {leaf} = leaf

setLeaf : t -> LeafWrapper t -> LeafWrapper t
setLeaf l t =
    { t | leaf = l }

mapLeafWrapper : (t->t) -> LeafWrapper t -> LeafWrapper t
mapLeafWrapper f t =
    { t | leaf = f t.leaf }

mapLeafWrapperWithEffects : (t->(t,a)) -> LeafWrapper t -> (LeafWrapper t, a)
mapLeafWrapperWithEffects f t =
    let (sm, sc) = f t.leaf in ({ t | leaf = sm }, sc)

{-|
-}
type alias Model number =
    { frame : Rect number
    , direction: N2.Axis
    }

withFrameRect : Rect number -> Model number
withFrameRect r =
    { frame = r
    , direction = N2.X
    }

withFullFrame : Model number
withFullFrame = withFrameRect unitRect



{-|
-}
type Msg
    = WindowSizeChanged Window.Size


css = ""

update msg model =
    case msg of
        WindowSizeChanged s ->
            ({ model | windowSize = s }, Cmd.none)


subscriptions model =
    Window.resizes WindowSizeChanged




toStyles : (number -> String) -> Rect number -> List (String, String)
toStyles unit {left, right, top, bottom} =
    [ ("position", "absolute")
    , ("top", unit top)
    , ("left", unit left)
    , ("width", unit <| right - left)
    , ("height", unit <| bottom - top)
    ]



frameView : Model number -> Rect number -> Html msg -> Html msg
frameView {frame} windowSize html =
    Html.div
        [ Html.Attributes.style <| toStyles (\v -> toString v ++ "px")
            <| rectToPixels (*) windowSize frame ]
        [ Html.text <| toString frame
        , html
        ]



------------------------------------


sumAxis : N2.Axis -> N2 number -> List (N2 number) -> N2 number
sumAxis axis init rs =
    List.foldl
        (\r sum -> N2.mapLAxis2 axis (+) sum r)
        init
        rs

axisFn : N2.Axis -> Rect a -> Range a
axisFn axis = case axis of
         N2.X -> Rect.xRange
         N2.Y -> Rect.yRange

foldAlong : (Range a -> b -> b) -> N2.Axis -> b -> List (Rect a) -> b
foldAlong fn axis init rs =
    List.foldl fn init (List.map (axisFn axis) rs)




--

sizesF : STree (Model Float) (LeafWrapper x) -> N2 Float -> N2 Float
sizesF tree init =
    let
        leafValue cursor {size} =
            N2.uniform <| size

        manyValue cursor {frame,direction} vals =
            vals
                |> Debug.log ("frame=" ++ toString frame ++ "vals=")
                |> sumAxis direction N2.zero
                |> N2.mapLAxis2 direction (+) (Rect.size frame)
                |> Debug.log ("sum=" )

    in
        STree.indexedMapAsTree leafValue manyValue init tree

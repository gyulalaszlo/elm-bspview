module Bsp.Root
    exposing
        ( subscriptions
        , update
        , view
        )

{-| Describe me please...
-}

import Array exposing (Array)
import Bsp.Cursor exposing (..)
import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Bsp.Model exposing (..)
import Bsp.Msg exposing (..)
import Bsp.Traits exposing (..)
import Bsp.Ratio exposing (Ratio(..))
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), SplitModel(Empty, Leaf, Node), SplitMeta, binary, directionToString, leaf)
import Dict exposing (Dict)
import Bsp.Error as Error exposing (Error)
import Html.Events exposing (onClick)
import List.Extra


-- SUBSCRIPTIONS


subscriptions : Model m l s e -> Sub (Msg m l)
subscriptions model =
    Sub.none



-- UPDATE


update : Msg m l -> Model m l s e -> ( Model m l s e, Cmd (Msg m l), Effects e )
update msg model =
    case msg of
        ChildMsg id m ->
            localModelFor CHead id model
                |> Maybe.map (\local -> model.traits.update m local)
                |> Maybe.map (\(cm, cc, ce) ->
                    ( { model | locals = (Dict.insert id cm model.locals)}
                    , cc
                    , ce
                    ))
                |> Maybe.withDefault (model, Cmd.none, Effects.none)

        SharedMsg m ->

             (model, Cmd.none, Effects.none)

        _ -> let

                (m,c) = updateBspOp msg model
             in (m, c, Effects.none)

updateBspOp : Msg m l -> Model m l s e -> ( Model m l s e, Cmd (Msg m l) )
updateBspOp msg model =
    case msg of
        Select c ->
            { model | cursor = c } ! []

        SplitAt c d l ->
            ( insertAt c d Equal l model
            , Cmd.none
            )

        SetLayoutEditingMode m ->
            { model | layoutEditingMode = m } ! []

        SwapLR c ->
            Bsp.SplitView.swapABAtCursor c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        SetDirection d c ->
            Bsp.SplitView.setDirectionAtCursor d c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        Rotate d c ->
            fromUpdateResult (Bsp.SplitView.rotateAtCursor d) c model

        RotateParent d c ->
            case Bsp.Cursor.parentCursor c of
                Nothing ->
                    ( model, Cmd.none )

                Just cc ->
                    fromUpdateResult (Bsp.SplitView.rotateAtCursor d) cc model

        DeleteAt c ->
            fromUpdateResult Bsp.SplitView.deleteAtCursor c model

        ResizeAt r c ->
            Bsp.SplitView.setRatioAt r c model.rootView
                |> Result.map (\v -> ( { model | rootView = v }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )

        _ -> Debug.crash "Cannot parse msg"


{-| do an `update()` using an update function
-}
fromUpdateResult : (Cursor -> SplitModel Id -> Result Error ( Cursor, SplitModel Id )) -> Cursor -> Model m l s e -> ( Model m l s e, Cmd (Msg m l) )
fromUpdateResult updateFn cursor model =
    updateFn cursor model.rootView
        |> Result.map (\( c, v ) -> ( { model | rootView = v, cursor = c }, Cmd.none ))
        |> Result.withDefault ( model, Cmd.none )



-- VIEW


view : Model m l s e -> Html (Msg m l)
view model =
    let
        context =
            case model.layoutEditingMode of
                NotEditingLayout -> WrappedContext WrappedNotSelected WrappedNotEditing

                EditingLayoutBlocks -> WrappedContext WrappedNotSelected WrappedIsEditing

        global = WrappedGlobal
                    { rootView = model.rootView
                    , cursor = model.cursor
                    , shared = model.shared
                    , selectedLeafId = model.selectedLeaf
                    , selectedLeafModel = (localModelAt model.cursor model) |> Result.toMaybe
                    }
    in
        div [ class "bsp-root-view" ]
            [ model.traits.wrapper context global
            , splitWrapper [ "root" ] [ treeSubView model identity model.rootView ]
            ]



-- VIEW: treeSubView


{-| the view of a tree node or leaf or empty node
-}
treeSubView : Model m l s e -> (Cursor -> Cursor) -> SplitModel Id -> Html (Msg m l)
treeSubView model cursorFn node =
    let
        recur =
            treeSubView model

        cursor =
            cursorFn CHead

        wrapper cls els =
            wrapperWithSelection model cursor cls els

--        nodeViewBaseTraits =
--            nodeViewBaseTraitsFor cursor model

        selectionMode =
            if model.cursor == cursor then WrappedIsSelected else WrappedNotSelected

        editingMode =
            case model.layoutEditingMode of
                    NotEditingLayout -> WrappedNotEditing
                    EditingLayoutBlocks -> WrappedIsEditing

        context =
            WrappedContext selectionMode editingMode

    in
        case node of
            Bsp.SplitView.Node meta ->
                let
                    { a, b, direction, ratio } =
                        meta

                    ( l, r ) =
                        splitAttrs direction ratio


                    nodeDiv v style dir el =
                        div [ prefixedClasses "node" [ "split", directionToString direction, v ]
                            , style
                            ]
                            [ recur (cursorFn << dir) el ]
                in
                    splitFor context cursorFn meta model <|
                        wrapper [ "node", directionToString direction ] <|
                            [ nodeDiv "a" l CLeft a
                            , nodeDiv "b" r CRight b
                            ]

            Bsp.SplitView.Leaf id ->
                    localModelFor cursor id model
                        |> Maybe.map (\mdl -> [leafFor context mdl model])
                        |> Maybe.withDefault []
                        |> wrapper [ "leaf" ]

            Bsp.SplitView.Empty ->
                wrapper [ "empty" ] [ emptyFor context cursor model ]


-- wrapper fns


emptyFor : WrappedContext -> Cursor -> Model m l s e -> Html (Msg m l)
emptyFor context cursor {traits,shared} =
    traits.wrapper context <|
        WrappedEmpty {shared = shared, cursor = cursor}


splitFor : WrappedContext -> (Cursor -> Cursor) -> SplitMeta Id -> Model m l s e -> Html (Msg m l) -> Html (Msg m l)
splitFor context cursorFn meta model el =
    model.traits.wrapper context <|
        WrappedNode
            { shared = model.shared
            , cursorFn = cursorFn
            , meta = meta
            , content = el
            }

leafFor : WrappedContext -> LocalModel m l s -> Model m l s e -> Html (Msg m l)
leafFor context localModel model =
    model.traits.wrapper context <|
        WrappedLeaf
            { model = localModel
            , view = model.traits.view
            }

-- WRAPPERS FOR BSP PANES ------------------------------------------------------


{-| Wraps a BSP view and adds the `selected` class bit if its selected
-}
wrapperWithSelection : Model m l s e -> Cursor -> List String -> List (Html msg) -> Html msg
wrapperWithSelection model cursor classArgs els =
    let
        classBits =
            classArgs
                ++ [ if (model.cursor == cursor) then
                        "selected"
                     else
                        "not-selected"
                   , case model.layoutEditingMode of
                       EditingLayoutBlocks -> "editing"
                       NotEditingLayout -> "not-editing"
                   ]

    in
        splitWrapper classBits els


{-| Wraps all BSP views with proper class names
-}
splitWrapper : List String -> List (Html msg) -> Html msg
splitWrapper classArgs els =
    div
        [ prefixedClasses "bsp-view-split-wrapper" classArgs
        , style
            [ ( "position", "absolute" )
            , ( "overflow", "hidden" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            ]
        ]
        els



-- VIEW: splitView


prefixedClasses : String -> List String -> Html.Attribute msg
prefixedClasses s ss =
    let prefix v = s ++ "-" ++ v
    in class <| String.join " " <| s :: (List.map prefix ss)


splitSize : Ratio -> ( Float, Float )
splitSize r =
    case r of
        Equal ->
            ( 50, 50 )

        FixedA v ->
            ( min 95 v, max 5 (100 - v) )

        FixedB v ->
            (  max 5 (100 - v), min 95 v )

{-| Returns the 'style' attribute for the left and right side for the given
Direction and Ratio.
-}
splitAttrs : Direction -> Ratio -> ( Html.Attribute msg, Html.Attribute msg )
splitAttrs direction ratio =
    let
        pct s v =
            ( s, toString v ++ "%" )

        pcts vv =
            List.map2 pct [ "left", "right", "top", "bottom" ] vv

        rect vv =
            ( "position", "absolute" ) :: (pcts vv)

        attrs fn =
            splitSize ratio
                |> fn
                |> (\( a, b ) -> ( style <| rect a, style <| rect b ))
    in
        case direction of
            Horizontal ->
                attrs (\( a, b ) -> ( [ 0, b, 0, 0 ], [ a, 0, 0, 0 ] ))

            Vertical ->
                attrs (\( a, b ) -> ( [ 0, 0, 0, b ], [ 0, 0, a, 0 ] ))

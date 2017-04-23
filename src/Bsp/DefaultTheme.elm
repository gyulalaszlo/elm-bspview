module Bsp.DefaultTheme
    exposing
        ( css
--        , toolbarTraits
        , normalTheme
        )

{-| Describe me please...
-}

import Bsp.Cursor exposing (Cursor(..), parentCursor)
import Bsp.Model exposing (Model)
import Bsp.Msg exposing (..)
import Bsp.Ratio exposing (Ratio(..))
import Bsp.Traits exposing (..)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), RotateDirection(..), SplitMeta, SplitModel(Node), nodeToString)
import Colors.Monokai
import Bsp.Css as Css
import Bsp.Error as Error exposing (Error)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed




-- REGULAR TOOLBARS ------------------------------------------------------------



normalTheme : (l -> String) -> (s -> List l) -> NodeViewWrapper m l s
normalTheme labelFn empties ctx viewToWrap =
    case viewToWrap of
        WrappedLeaf { model, view } -> normalThemeLeaf labelFn ctx view model

        WrappedNode split ->
            case ctx.edited of
                WrappedNotEditing -> split.content
                WrappedIsEditing -> normalThemeEditedSplit ctx split

        WrappedEmpty empty -> emptyBase labelFn empties empty

        WrappedGlobal global ->
            normalThemeToolbar labelFn empties ctx global



emptyBase labelFn empties {cursor, shared} =
    Html.div
        []
        [ Html.Keyed.ul [] <|
            List.indexedMap (\i l-> (toString i, l)) <|
            List.map (\l -> Html.li [] [ btn (SplitAt cursor Horizontal l) <| labelFn l] )
                (empties shared)
        ]




normalThemeToolbar labelFn empties ctx {rootView, cursor, shared, selectedLeafId, selectedLeafModel} =
    div [ class "global-toolbar" ]
        [ case ctx.edited of
            WrappedNotEditing -> btn (SetLayoutEditingMode EditingLayoutBlocks) "Edit layout"
            WrappedIsEditing -> btn (SetLayoutEditingMode NotEditingLayout) "Done"
        , text " | "
        , text <| Bsp.SplitView.nodeToString rootView
        , text " | "
        , case selectedLeafModel of
            Nothing -> text "No leaf selected"
            Just model -> toolbarLeaf labelFn empties model
        ]


normalThemeLeaf labelFn ctx view model =
    div [ class "normal-leaf" ]
        [ div [ class "wrapped-leaf" ] [ view model ]
        , leafToolbar labelFn ctx view model
        ]


leafToolbar labelFn ctx view model =
    let closeBtn = span [ class "close-button" ] [ btn (DeleteAt model.cursor ) "×" ]
        label = span [ class "label" ] [ text <| labelFn model.local ]
        id = span [ class "id" ] [ text <| toString model.id ]
        leafClass cl = class <| "leaf-id leaf-id-" ++ cl
        leafAttrs  = case ctx.selected of
            WrappedNotSelected ->
                [ leafClass "not-selected", onClick (Select model.cursor) ]
            WrappedIsSelected ->
                [ leafClass "selected" ]

    in div leafAttrs [ closeBtn, id , label ]



-- SPLIT

normalThemeEditedSplit ctx { cursorFn, shared, meta, content } =
    case ctx.selected of
        WrappedNotSelected ->
            div [ class "layout-editing-split-wrapper  layout-editing-split-wrapper-not-selected" ]
                [ normalThemeEditedSplitView ctx cursorFn meta shared
                , div [class "layout-editing-inner-wrap" ] [ content ]
                ]
        WrappedIsSelected ->
            div [ class "layout-editing-split-wrapper layout-editing-split-wrapper-selected" ]
                [ normalThemeEditedSplitView ctx cursorFn meta shared
                , div [class "layout-editing-inner-wrap" ] [content ]]


normalThemeEditedSplitView : WrappedContext -> (Cursor -> Cursor) -> SplitMeta Id -> s -> Html (Msg m l)
normalThemeEditedSplitView ctx cursorFn { a, b, direction, ratio } _ =
    let
        cursor =
            cursorFn CHead

        dirBtn dir label =
            btn (SetDirection dir cursor) label

        resizeBtn r label = btn (ResizeAt r cursor) label

        canRotate =
            case ( a, b ) of
                ( _, Node _ ) ->
                    True

                ( Node _, _ ) ->
                    True

                _ ->
                    False
    in
        div (case ctx.selected of
                WrappedNotSelected ->
                    [ class "layout-editing-split-view layout-editing-split-not-selected"
                    , onClick (Select cursor)
                    ]
                _ -> [ class "layout-editing-split-view layout-editing-split-selected" ])
            [ text <| " A=" ++ nodeToString a
            , text <| " B=" ++ nodeToString b
            , case ctx.selected of
                WrappedNotSelected -> text ""
                WrappedIsSelected ->
                    span []
                        [ btn (Rotate CW cursor) <| "<-R"
                        , btn (Rotate CCW cursor) <| "R->"
                        , btn (SwapLR cursor) <| "AB -> BA"
                        , case direction of
                            Vertical ->
                                dirBtn Horizontal "-- H --"

                            Horizontal ->
                                dirBtn Vertical "|| V ||"
                        , resizeBtn (FixedB 66) "1+2"
                        , resizeBtn (Equal) "=="
                        , resizeBtn (FixedA 66) "1+2"
                        ]
            ]

-- TOOLBAR --

toolbarLeaf labelFn emptiesFn mdl =
    let {cursor,id, local, shared} = mdl
        empties = emptiesFn shared
    in
        [ List.map (\l -> btn (SplitAt cursor Horizontal l) <| "|| " ++ labelFn l) empties
        , List.map (\l -> btn (SplitAt cursor Vertical l) <| "-- " ++ labelFn l) empties
        ] ++ parentToolbar labelFn empties mdl
            |> List.intersperse ([text " | "])
            |> List.concat
            |> span [ class "toolbar-leaf" ]



parentToolbar labelFn empties {cursor, id, local} =
    case Bsp.Cursor.parentCursor cursor of
        Nothing -> []
        Just cc ->
            [
                [ text "Rot "
                , btn (Rotate CCW cc) <| "<-"
                , btn (Rotate CW cc) <| "->"
                ]
            ,
                [ text "RotP "
                , btn (RotateParent CW cc) <| "<=="
                , btn (RotateParent CCW cc) <| "==>"
                ]
            ,   [ btn (SwapLR cc) <| "<->"
                ]
            ,   [ btn (SetDirection Horizontal cc) "| Hor |"
                , btn (SetDirection Vertical cc) "- Ver -"
                ]
            ,   [ btn (ResizeAt (FixedB 66) cc) "66/33"
                , btn (ResizeAt Equal cc) "50/50"
                , btn (ResizeAt (FixedA 66) cc) "33/66"
                ]
            ]





btn : Msg m l -> String -> Html (Msg m l)
btn click label =
    Html.button [ onClick click ] [ text label ]



-- CSS -------------------------------------------------------------------------


{-| CSS parts for childView
-}
css : String
css =
    let themeColor cfn = (Css.color << cfn) Colors.Monokai.theme
    in Css.css
        [ ( "leaf-background", themeColor .background )
        , ( "leaf-text", themeColor .text )
        , ( "leaf-selection", themeColor .selection )
        , ( "split-background", "black" )
        , ( "leaf-radius", "5px" )
        , ( "leaf-margin", "0.2em" )

        , ( "color-darkbg", Css.color <| Colors.Monokai.darkBlack)

        -- layout-editing
        , ( "layout-border-size", "0.3em" )
        , ( "layout-margin", "0.3em" )
        , ( "layout-editing-margin", "0.3em" )

        , ( "layout-editing-split-border-style", "0.1em dotted" )
        -- toolbar data
        , ( "toolbar-margin", "0.3em" )
        , ( "toolbar-height", "2em" )
        ]
        """
.child-view {  }

.btn { display:inline-block; padding: 0.3em 1em; cursor: pointer; font-size:0.8em;}
.btn:hover { text-decoration:underline; }

.bsp-root-view { background: {{ split-background }}; color: white; position: absolute; left: 0; right:0; top:0; bottom:0; }

/* LEAF BASICS --------------------------- */

.bsp-view-split-wrapper-leaf { margin: {{ leaf-margin }}; }
.bsp-view-split-wrapper-leaf.bsp-view-split-wrapper-not-selected { background-color: {{ color-darkbg }}; color: #ccc; }
.bsp-view-split-wrapper-leaf.bsp-view-split-wrapper-selected { background-color: {{ leaf-background }}; color: {{ leaf-text }}; border-top-color: {{ leaf-selection }}; }

/* NODE WRAPPER -------------------------- */

.bsp-view-split-wrapper { border-radius: {{ leaf-radius }}; }

.layout-editor-global-header,
.bsp-view-node-split-horizontal-toolbar,
.bsp-view-node-split-vertical-toolbar { position:absolute; top:0; left: 0; right: 0; z-index:999; }


.bsp-view-split-wrapper {}


/* LAYOUT EDITING ===================================== */

.bsp-view-split-wrapper-editing { margin: {{ layout-editing-margin }}; }
.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-leaf { background: {{ leaf-background }}; }
.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-node {}

.bsp-view-split-wrapper-editing.bsp-view-split-wrapper-selected { border-color: {{ leaf-selection }}; }


.a--bsp-view-split-wrapper-editing .bsp-view-node-split-horizontal,
.a--bsp-view-split-wrapper-editing .bsp-view-node-split-vertical { margin-top: {{ toolbar-height }}; }

.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-vertical-toolbar,
.bsp-view-split-wrapper-editing-layout .bsp-view-node-split-horizontal-toolbar { margin: {{ toolbar-margin }}; }



.bsp-view-split-wrapper-root { margin-top: {{ toolbar-height }}; }


.layout-editing-leaf-label { position: absolute; font-size: 3em; color: #888; bottom: 0; left: 0; right: 0; text-align: center; cursor: pointer; }
.layout-editing-leaf-label:hover { color: {{ leaf-selection }}; }


/* -- Split editor ----------------------------- */

.layout-editing-split-wrapper {  border: {{ layout-border-size }} solid {{ split-background }}; background: {{ leaf-background }}; border-radius: {{ leaf-radius }}; margin: {{ layout-editing-margin }};  background-color: {{ color-darkbg }};  }
.layout-editing-split-wrapper-selected { border-color: {{ leaf-selection }}; }

.layout-editing-split-wrapper { position: absolute; top: 0; left: 0; right:0; bottom: 0; overflow: hidden; }

.bsp-view-split-wrapper-selected.bsp-view-split-wrapper-editing > .node.node-split.node-b { border-color: {{ leaf-selection }}; }

.layout-editing-split-view.layout-editing-split-selected { background: {{ leaf-selection }}; }
.layout-editing-split-view { background: {{ split-background }};  cursor: pointer; }
.layout-editing-split-view-not-selected { cursor: pointer; }
.layout-editing-split-view-not-selected:hover { color: {{ leaf-selection }};}

/* -- SPLIT HEADER -- */

.layout-editing-split-view { position: absolute; top: 0; height: {{ toolbar-height }}; left: 0; right:0;  overflow: hidden; text-align:center; }

/* -- WRAP -- */

.layout-editing-inner-wrap { position: absolute; top: {{ toolbar-height }}; left: 0; right:0; bottom: 0; }


.leaf-id { font-size: 0.8em; position: absolute; left: 0; right: 0; top: 0; background: {{ color-darkbg }}; color: {{ leaf-background }};  padding: 0.2em 1em; border-bottom: 0.2em solid black; }

.leaf-id-selected { background-color: {{ leaf-selection }};  }
.leaf-id-not-selected { cursor:pointer;}

.leaf-id .id { margin-right: 1.2em }
.leaf-id .id:before { content: "#"; }
.leaf-id .label { font-weight:bold; cursor:pointer; }
.leaf-id-not-selected:hover .id { color: {{ leaf-background }}; }
.leaf-id-not-selected .label:hover { text-decoration:underline; }
.leaf-id .close-button button { border:none; font-weight:bold; }
.leaf-id-selected .close-button button { color: {{ leaf-background }}; }
.leaf-id .close-button button:hover { }

.normal-leaf {}
.wrapped-leaf { position:absolute; top: 2em; left:0; right:0; bottom:0; overflow-x: hidden; overflow-y:scroll; }



/* Scrollbar styles */
::-webkit-scrollbar { width: 12px; height: 12px; }
::-webkit-scrollbar-track { background: none; border-radius: 10px; }
::-webkit-scrollbar-thumb { border-radius: 10px; background: rgba(255,255,255,0.1);  }
::-webkit-scrollbar-thumb:hover { background: rgba(0,0,0,0.1);  }
"""

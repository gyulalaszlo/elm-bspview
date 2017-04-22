module Bsp.SplitView
    exposing
        ( SplitModel(..)
        , Direction(..)
        , SplitMeta
        , directionToString
        , swapAB
        , at
        , valueAt
        , empty
        , leaf
        , binary
        , horizontal
        , vertical
        , splitAtCursor
        , swapABAtCursor
        , setDirectionAtCursor
        , setRatioAt
        , rotateAtCursor
        , deleteAtCursor
        , RotateDirection(..)
        , nodeToString
        )

{-| Describe me please...
-}

import Bsp.Cursor exposing (BspStep(Left, Right), Cursor(..))
import Bsp.Ratio exposing (Ratio)
import Color exposing (Color)
import Bsp.Css as Css exposing (..)
import Bsp.Error as Error exposing (Error)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)


-- MODEL


type Direction
    = Horizontal
    | Vertical


directionToString : Direction -> String
directionToString d =
    case d of
        Horizontal ->
            "horizontal"

        Vertical ->
            "vertical"


{-| Represents a node in a BSP hierarchy
-}
type SplitModel v
    = Node (SplitMeta v)
    | Leaf v
    | Empty


type alias SplitMeta v =
    { a : SplitModel v
    , b : SplitModel v
    , direction : Direction
    , ratio : Ratio
    }


{-| Creates a new empty leaf
-}
empty : SplitModel v
empty =
    Empty


{-| Creates a new leaf node for the BSP tree from an initial view.
-}
leaf : v -> SplitModel v
leaf v =
    Leaf v


{-| Concatenates two nodes into a node
-}
binary : Direction -> Ratio -> SplitModel v -> SplitModel v -> SplitModel v
binary d r a b =
    Node { a = a, b = b, direction = d, ratio = r }


horizontal : Ratio -> SplitModel v -> SplitModel v -> SplitModel v
horizontal =
    binary Horizontal


vertical : Ratio -> SplitModel v -> SplitModel v -> SplitModel v
vertical =
    binary Vertical



-- Specialized folding types


type alias ViewAndCursor v =
    ( Cursor, SplitModel v )


type alias UpdateResult v =
    Result Error (ViewAndCursor v)


type alias CursorFn =
    Cursor -> Cursor


{-|
-}
type alias CursorUpdateFn v =
    CursorFn -> SplitModel v -> UpdateResult v



-- VALUE AT --------------------------------------------------------------------


at : Cursor -> SplitModel v -> Result Error (SplitModel v)
at c v =
    Bsp.Cursor.at stepIntoSplitView c v


valueAt : Cursor -> SplitModel v -> Result Error v
valueAt c v =
    case at c v of
        Ok (Leaf v) ->
            Ok v
        Ok (_) ->
            Error.errMsg [ "Cannot get value from node:", toString v ]

        Err e ->
            e
                |> Error.wrapMsg ["in valueAt  ", toString c, toString v]
                |> Err



-- NODE META OPERATIONS WITH MAYBE ---------------------------------------------


nodeMeta : SplitModel v -> Maybe (SplitMeta v)
nodeMeta v =
    case v of
        Node n ->
            Just n

        _ ->
            Nothing


mapNodeMeta : (SplitMeta v -> SplitMeta v) -> SplitModel v -> Maybe (SplitModel v)
mapNodeMeta fn v =
    nodeMeta v |> Maybe.map (\n -> Node <| fn n)



-- NODE META OPERATIONS WITH RESULT X ------------------------------------------


mapNodeMetaOp : (SplitModel v -> x) -> (SplitMeta v -> SplitMeta v) -> SplitModel v -> Result x (SplitModel v)
mapNodeMetaOp err fn v =
    mapNodeMeta fn v |> Result.fromMaybe (err v)


attemptNodeMetaOp : (SplitModel v -> x) -> (SplitMeta v -> Result x (SplitMeta v)) -> SplitModel v -> Result x (SplitModel v)
attemptNodeMetaOp err fn v =
    nodeMeta v
        |> Result.fromMaybe (err v)
        |> Result.andThen fn
        |> Result.map Node



-- SWAP A/B -> B/A -------------------------------------------------------------


swapAB : SplitModel v -> Maybe (SplitModel v)
swapAB n =
    mapNodeMeta swapABMeta n


swapABMeta : SplitMeta v -> SplitMeta v
swapABMeta n =
    { n | a = n.b, b = n.a, ratio = Bsp.Ratio.flipRatio n.ratio }



-- CURSOR FOLDING TRAITS -------------------------------------------------------


stepIntoSplitView : Bsp.Cursor.BspStep -> SplitModel v -> Result Error ( SplitModel v, SplitModel v -> SplitModel v )
stepIntoSplitView dir v =
    case ( v, dir ) of
        ( Node n, Left ) ->
            Ok ( n.a, (\aa -> Node { n | a = aa }) )

        ( Node n, Right ) ->
            Ok ( n.b, (\bb -> Node { n | b = bb }) )

        _ ->
            Error.errMsg [ "Cannot step in direction", toString dir, "into", toString v ]


stepIntoViewAndCursor : Bsp.Cursor.BspStep -> ViewAndCursor v -> Result Error ( ViewAndCursor v, ViewAndCursor v -> ViewAndCursor v )
stepIntoViewAndCursor dir r =
    let
        innerMap ff c v =
            Ok ( ( c, v ), (\( cc, vv ) -> ff cc vv) )

        fn ( c, v ) =
            case ( v, c, dir ) of
                ( Node n, CLeft c, Left ) ->
                    innerMap (\cc aa -> ( CLeft cc, Node { n | a = aa } )) c n.a

                ( Node n, CRight c, Right ) ->
                    innerMap (\cc bb -> ( CRight cc, Node { n | b = bb } )) c n.b

                _ ->
                    Error.errMsg [ "Cannot step in direction", toString dir, "into", toString v ]
    in
        fn r
            |> Debug.log "stepInto"



-- SPECIALIZED FOLDS FOR NODE META


{-| operate on the tip of the cursor and return a SplitView Result
-}
foldWithStepInto : (SplitModel v -> Result Error (SplitModel v)) -> Cursor -> SplitModel v -> Result Error (SplitModel v)
foldWithStepInto fn c v =
    Bsp.Cursor.foldCursor fn stepIntoSplitView v c


attemptNodeMetaOpAt : (Cursor -> SplitModel v -> Error) -> (SplitMeta v -> Result Error (SplitMeta v)) -> Cursor -> SplitModel v -> Result Error (SplitModel v)
attemptNodeMetaOpAt err fn c v =
    let
        ffn vv =
            attemptNodeMetaOp (err c) fn vv
    in
        foldWithStepInto ffn c v


mapNodeMetaAt : String -> (SplitMeta v -> SplitMeta v) -> Cursor -> SplitModel v -> Result Error (SplitModel v)
mapNodeMetaAt errLabel fn c v =
    let
        err c v =
            Error.makeMsg [ "Cannot", errLabel, "for cursor:", toString c, "in node:", toString v ]

        ffn vv =
            mapNodeMetaOp (err c) fn vv
    in
        foldWithStepInto ffn c v



-- SPECIALIZED FOLDS FOR NODE META


{-| operate on the tip of the cursor and return a SplitView Result
-}
foldViewAndCursor : (ViewAndCursor v -> Result Error (ViewAndCursor v)) -> Cursor -> SplitModel v -> UpdateResult v
foldViewAndCursor fn c v =
    Bsp.Cursor.foldCursor fn stepIntoViewAndCursor ( c, v ) c


andThenCleanTree : Result x (ViewAndCursor v) -> Result x (ViewAndCursor v)
andThenCleanTree r =
    Result.map (\( cc, v ) -> ( cc, cleanTree v )) r



-- BSP VIEW TREE OPERATIONS ----------------------------------------------------


splitAtCursor : Direction -> Ratio -> v -> Cursor -> SplitModel v -> Result Error ( Cursor, SplitModel v )
splitAtCursor direction ratio id cursor node =
    let
        splitInner ( c, v ) =
            case v of
                Empty ->
                    Ok <| ( c, leaf id )

                _ ->
                    Ok <| ( CRight c, binary direction ratio v <| leaf id )
    in
        foldViewAndCursor splitInner cursor node



-- OPERATIONS WITH CURSORS -----------------------------------------------------


{-| Swaps the Left and Right nodes at the cursor
-}
swapABAtCursor : Cursor -> SplitModel v -> Result Error (SplitModel v)
swapABAtCursor c v =
    mapNodeMetaAt "swap A / B" swapABMeta c v



-- DIRECTION


{-| Sets the direction of a split
-}
setDirectionAtCursor : Direction -> Cursor -> SplitModel v -> Result Error (SplitModel v)
setDirectionAtCursor d c v =
    mapNodeMetaAt "set split direction" (\n -> { n | direction = d }) c v



-- RATIO


{-| Sets the direction of a split
-}
setRatioAt : Ratio -> Cursor -> SplitModel v -> Result Error (SplitModel v)
setRatioAt ratio c v =
    mapNodeMetaAt "set split ratio" (\n -> { n | ratio = ratio }) c v



-- DELETE ----------------------------------------------------------------------


{-| Replaces the target BSP node with an empty node then cleans the tree
-}
deleteAtCursor : Cursor -> SplitModel v -> UpdateResult v
deleteAtCursor c v =
    foldViewAndCursor (always <| Ok ( CHead, Empty )) c v
        |> andThenCleanTree


{-| Cleans the tree be merging Empty leaves with leafs and nodes
-}
cleanTree : SplitModel v -> SplitModel v
cleanTree v =
    case v of
        Node nm ->
            case ( cleanTree nm.a, cleanTree nm.b ) of
                ( Empty, Empty ) ->
                    Empty

                ( aa, Empty ) ->
                    aa

                ( Empty, bb ) ->
                    bb

                ( aa, bb ) ->
                    Node { nm | a = aa, b = bb }

        _ ->
            v



-- ROTATION --------------------------------------------------------------------


{-| Rotate changes the views without modifying the splits on their level
-}
type RotateDirection
    = CW
    | CCW


rotate : RotateDirection -> SplitMeta v -> Result Error (SplitMeta v)
rotate dir n =
    let
        err =
            Error.errMsg [ "Cannot rotate node", toString n ]

        update n aa bb =
            Node { n | a = aa, b = bb }
    in
        case ( dir, n.a, n.b ) of
            -- Clockwise
            ( CW, Node na, Node nb ) ->
                Ok
                    { n
                        | a = update na na.b nb.b
                        , b = update nb na.a nb.a
                    }

            ( CW, Node na, b ) ->
                Ok { n | a = update na na.b b, b = na.a }

            ( CW, a, Node nb ) ->
                Ok { n | a = nb.b, b = update nb a nb.a }

            -- Counter-Clockwise
            ( CCW, Node na, Node nb ) ->
                Ok
                    { n
                        | a = update na nb.a na.a
                        , b = update nb nb.b na.b
                    }

            ( CCW, Node na, b ) ->
                Ok { n | a = update na b na.a, b = na.b }

            ( CCW, a, Node nb ) ->
                Ok { n | a = nb.a, b = update nb nb.b a }

            -- Other things
            _ ->
                Ok { n | a = n.b, b = n.a }



--            flipNode n
--                |> Result.fromMaybe (Error.makeMsg ["Cannot rotate node", toString n])


{-| Rotates the node at the cursor
-}
rotateAtCursor : RotateDirection -> Cursor -> SplitModel v -> UpdateResult v
rotateAtCursor dir c v =
    let
        err c v =
            Error.makeMsg [ "Cannot find node at:", toString c, "in", toString v ]

        fn ( cc, v ) =
            attemptNodeMetaOp (err c) (rotate dir) v
                |> Result.map (\v -> ( cc, v ))
    in
        Bsp.Cursor.foldCursor fn stepIntoViewAndCursor ( c, v ) c



-- TO STRING HELPERS -----------------------------------------------------------


nodeToString : SplitModel v -> String
nodeToString n =
    case n of
        Leaf v ->
            toString v

        Node { a, b } ->
            "[ " ++ nodeToString a ++ ", " ++ nodeToString b ++ " ]"

        Empty ->
            "<Empty>"


debugNode n =
    let
        dd =
            Debug.log (nodeToString n) ""
    in
        n

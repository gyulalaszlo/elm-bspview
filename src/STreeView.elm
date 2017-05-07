module STreeView exposing
    (.. )
{-| Describe me please...
-}

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import STree exposing (Cursor, STree)
import STreeSplit
import Units.Compose.Range as Range exposing (Range)
import Units.Compose.N2 as N2 exposing (N2)
import Units.Compose.Rect as Rect exposing (Rect)



-- MODEL

type alias Model v =
    { tree: STree (STreeSplit.Model Float) (STreeSplit.LeafWrapper v)
    }


initialModel : Model v
initialModel =
    { tree = STree.empty
    }


insertAt : Cursor -> v -> Model v -> Model v
insertAt c v m =
    STree.insertAt c (STreeSplit.withFullFrame) (STreeSplit.wrapLeaf v) m.tree
        |> Result.map (\v -> { m | tree = v})
        |> Result.mapError (Debug.log "insertAt error=")
        |> Result.withDefault m

-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : (Cursor -> v -> Sub msg) -> Model v -> Sub msg
subscriptions subFn model =
    STree.indexedMapAsTree (\cursor v -> subFn cursor <| STreeSplit.leaf v ) (\_ _ -> Sub.batch) Sub.none model.tree

--    STree.mapAsTree (subFn ) Sub.batch model.tree



-- UPDATE



update : (msg -> v -> (v, Cmd msg)) -> Cursor -> msg -> Model v -> (Model v, Cmd msg)
update childUpdateFn k msg model =
    let innerUpdate = STreeSplit.mapLeafWrapperWithEffects (childUpdateFn msg)
    in STree.mapAtWithEffects innerUpdate k model.tree
        |> Result.map (\(sm,sc) -> ({model | tree = sm}, sc))
        |> Result.mapError (Debug.log "Err=")
        |> Result.withDefault (model, Cmd.none)

-- VIEW


view : (Cursor -> v -> Html msg) -> Model v -> Html msg
view childViewFn model =
    let windowSize = Rect.from 0 1024 0 768
        child i v =  childViewFn i  <| STreeSplit.leaf v
        mergeView i meta html =
            div []
                [ div [] html
                , Html.text <| "sum=" ++ toString units
                , Html.text <| " meta=" ++ toString meta
                ]

        axis = N2.X

        units = STreeSplit.sizesF model.tree N2.zero
--            STree.indexedMapAsTree
--                (\_ _ -> N2.uniform 0)
--                (\_ {frame} vals ->
--                    STreeSplit.sumAxis N2.X (N2.uniform 0) vals
--                        |> N2.mapLAxis2 N2.X (+) (Rect.size frame)
--                        )
--                (N2.uniform 0)
--                model.tree

    in STree.indexedMapAsTree child mergeView (Html.text "") model.tree




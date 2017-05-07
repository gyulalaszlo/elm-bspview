module STreeView exposing
    (.. )
{-| Describe me please...
-}

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import STree exposing (Cursor, STree)
import Units.Compose.Range as Range exposing (Range)
import Units.Compose.N2 as N2 exposing (N2)



-- MODEL


type alias Model v =
    { tree: STree v
    }


initialModel : Model v
initialModel =
    { tree = STree.empty
    }


insertAt : Cursor -> v -> Model v -> Model v
insertAt c v m =
    STree.insertAt c v m.tree
        |> Result.map (\v -> { m | tree = v})
        |> Result.mapError (Debug.log "insertAt error=")
        |> Result.withDefault m

-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : (Cursor -> v -> Sub msg) -> Model v -> Sub msg
subscriptions subFn model =
    STree.indexedMapAsTree (subFn) (\_ -> Sub.batch) model.tree

--    STree.mapAsTree (subFn ) Sub.batch model.tree



-- UPDATE



update : (msg -> v -> (v, Cmd msg)) -> Cursor -> msg -> Model v -> (Model v, Cmd msg)
update childUpdateFn k msg model =
    STree.mapAtWithEffects (childUpdateFn msg) k model.tree
        |> Result.map (\(sm,sc) -> ({model | tree = sm}, sc))
        |> Result.mapError (Debug.log "Err=")
        |> Result.withDefault (model, Cmd.none)

-- VIEW


view : (Cursor -> v -> Html msg) -> Model v -> Html msg
view view model =
    STree.indexedMapAsTree view (\_ -> div [])  model.tree




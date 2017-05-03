module STreeView exposing
    (.. )
{-| Describe me please...
-}

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import STree exposing (Cursor, STree)


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


subscriptions : Model v -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


--update : Msg -> Model v -> (Model v, Cmd Msg)
--update msg model =
--    case msg of
--        Noop -> model ! []

update : (msg -> v -> (v, Cmd msg)) -> STree.Cursor -> msg -> Model v -> (Model v, Cmd msg)
update childUpdateFn k msg model =
    STree.mapWithEffects (\mdl -> childUpdateFn msg mdl) k model.tree
        |> Result.map (\(sm,sc) -> ({model | tree = sm}, sc))
        |> Result.mapError (Debug.log "Err=")
        |> Result.withDefault (model, Cmd.none)
--    let (sm,sc) = STree.mapWithEffects (\mdl -> childUpdateFn msg mdl) k model.tree
--    in ({ model | tree = sm }, sc)
--    Dict.get k model.views
--        |> Maybe.map (\mdl -> childUpdateFn msg mdl)
--        |> Maybe.map (\(sm, sc)-> ({ model | views = Dict.insert k sm model.views }, sc))
--        |> Maybe.withDefault (model, Cmd.none)

-- VIEW


view : (v -> Html msg) -> Model v -> Html msg
view msg model =
    div [ class "STreeView-view" ]
        [ text <| toString model ]
        
        
-- CSS

css : String
css = """
.STreeView-view {}
"""
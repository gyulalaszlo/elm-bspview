module Bsp.Model
    exposing
        ( Model
        , nextId
        , insertAt
        , modelFrom
        , localModelFor
        , sharedModelFor
        , localModelAt

        , getShared
        , setShared
        , mapShared
        , attemptMapShared
        )

{-| Describe me please...
-}

-- ID --------------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor(..))
import Bsp.Msg exposing (Id, LayoutEditingMode(EditingLayoutBlocks, NotEditingLayout), Msg(ChildMsg))
import Bsp.Ratio exposing (Ratio)
import Bsp.SplitView exposing (Direction(..), SplitMeta, SplitModel(..), binary, leaf, splitAtCursor)
import Bsp.Traits exposing (LocalModel, SharedModel, Traits)
import Bsp.Error as Error exposing (Error)
import Dict exposing (Dict)
import Html exposing (Html)


nextId : Id -> Id
nextId old =
    old + 1



-- MODEL & CONSTRUCTORS --------------------------------------------------------


type alias Model msg local shared effects =
    { shared : shared
    , locals : Dict Id local
    , traits : Traits msg local shared effects
    , rootView : SplitModel Id
    , cursor : Cursor
    , nextId : Id
    , layoutEditingMode : LayoutEditingMode
    , selectedLeaf : Maybe Id
    }


{-| Creates a new Bsp Root View
-}
modelFrom : Traits m l s e -> s -> Model m l s e
modelFrom traits shared =
    { shared = shared
    , locals = Dict.empty
    , traits = traits
    , rootView = Bsp.SplitView.Empty
    , cursor = CHead
    , nextId = 0
    , layoutEditingMode = NotEditingLayout
    , selectedLeaf = Nothing
    }


{-| Returns Just a LocalModel for an Id and Cursor or Nothing if the id is not in the locals.
-}
localModelFor : Cursor -> Id -> Model m l s e -> Maybe (LocalModel m l s)
localModelFor cursor id { locals, shared } =
    Dict.get id locals
        |> Maybe.map (\local -> LocalModel local shared cursor id (ChildMsg id))


localModelAt : Cursor -> Model m l s e -> Result Error (LocalModel m l s)
localModelAt c model =
    let
        err id =
            Error.makeMsg [ "Cannot find model for id:", toString id, "for cursor:", toString c ]

        localModelForId id =
            localModelFor model.cursor id model
                |> Result.fromMaybe (err id)

        localView =
            Bsp.SplitView.valueAt model.cursor model.rootView
                |> Result.andThen localModelForId
    in
        localView


sharedModelFor : (Cursor -> Cursor) -> SplitMeta Id -> s -> SharedModel s
sharedModelFor cFn meta s =
    { shared = s, cursor = cFn, meta = meta }



-- SHARED MODEL MESSING WITH ---------------------------------------------------

getShared : Model m l s e -> s
getShared { shared } = shared

setShared : s -> Model m l s e -> Model m l s e
setShared s model = { model | shared = s }


mapShared : (s -> s) -> Model m l s e -> Model m l s e
mapShared mapper model =
    { model | shared = mapper model.shared }

attemptMapShared : (s -> Result x s) -> Model m l s e -> Result x (Model m l s e)
attemptMapShared mapper model =
    mapper model.shared
        |> Result.map (\ss -> { model | shared = ss })



-- CURSOR AND CURSOR OPERATIONS ------------------------------------------------


type alias ViewNode =
    SplitModel Id


insertLocal : l -> Model m l s e -> Model m l s e
insertLocal local model =
    let
        id =
            model.nextId
    in
        { model
            | nextId = nextId id
            , locals = Dict.insert id local model.locals
        }


insertAt : Cursor -> Direction -> Ratio -> l -> Model m l s e -> Model m l s e
insertAt cursor direction ratio local model =
    splitAtCursor direction ratio model.nextId cursor model.rootView
        |> Result.map (\( cc, newRoot ) -> { model | rootView = newRoot, cursor = cc })
        |> Result.map (insertLocal local)
        |> Result.withDefault model

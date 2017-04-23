module Bsp.Traits exposing (..)

{-| Describe me please...
-}

-- TRAITS ----------------------------------------------------------------------

import Bsp.Cursor exposing (Cursor)
import Bsp.Msg exposing (Id, Msg)
import Bsp.SplitView exposing (SplitMeta, SplitModel)
import Bsp.Error as Error exposing (Error)
import Effects exposing (Effects)
import Html exposing (Html)


type alias LocalModel msg local shared =
    { local : local
    , shared : shared
    , cursor : Cursor
    , id : Id
    , msg : msg -> Msg msg local
    }


type alias SharedModel shared =
    { shared : shared
    , cursor : Cursor -> Cursor
    , meta : SplitMeta Id
    }


--------------------------------------------------------------------------------
-- View Wrapper models


type alias LeafWrapperModel m l s =
    { model: LocalModel m l s
    , view: LocalModel m l s -> Html (Msg m l)
    }

type alias GlobalWrapperModel m l s =
    { rootView : SplitModel Id
    , cursor: Cursor
    , shared: s
    , selectedLeafId : Maybe Id
    , selectedLeafModel: Maybe (LocalModel m l s)
    }


type alias EmptyWrapperModel s =
    { shared: s
    , cursor: Cursor
    }

type alias NodeWrapperModel m l s =
    { shared : s
    , cursorFn : Cursor -> Cursor
    , meta : SplitMeta Id
    , content: Html (Msg m l)
    }

type WrappedNodeView m l s
    = WrappedLeaf (LeafWrapperModel m l s)
    | WrappedNode (NodeWrapperModel m l s)
    | WrappedEmpty (EmptyWrapperModel s)
    | WrappedGlobal (GlobalWrapperModel m l s)


--------------------------------------------------------------------------------
-- CONTEXT

type WrappedSelection
    = WrappedIsSelected
    | WrappedNotSelected

type WrappedEditMode
    = WrappedIsEditing
    | WrappedNotEditing

type alias WrappedContext =
    { selected: WrappedSelection
    , edited: WrappedEditMode
    }


type alias WrapperFnBase ctx m l s = ctx -> WrappedNodeView m l s -> Html (Msg m l)

type alias NodeViewWrapper m l s = WrapperFnBase WrappedContext m l s



-- =======

--type alias EmptyViewFn msg local shared =
--    Cursor -> shared -> Html (Msg msg local)
--
--
--type alias GlobalViewFn msg local shared =
--    shared -> Result Error (LocalModel msg local shared) -> Html (Msg msg local)


type alias LocalViewFn msg local shared =
    LocalModel msg local shared -> Html (Msg msg local)



type alias Traits msg local shared effects =
    { subscriptions : LocalModel msg local shared -> Sub msg
    , update : msg -> LocalModel msg local shared -> ( local, Cmd (Msg msg local), Effects effects)
    , view : LocalViewFn msg local shared
    , wrapper :NodeViewWrapper msg local shared
    }






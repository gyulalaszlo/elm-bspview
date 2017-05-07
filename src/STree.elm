module STree exposing (..)
{-| Describe me please...
-}

import List.Extra

type STree v2 v1
    = Zero
    | One v1
    | Many v2 (List (STree v2 v1))

empty = Zero


list = Many
concat = Many
leaf = One


fromList : m -> List v -> STree m v
fromList m = Many m << List.map One



childrenOf : STree m v -> List (STree m v)
childrenOf t =
    case t of
        Zero -> []
        One v -> []
        Many _ vs -> vs

-------------------------------------------------------------------------

type InsertPos
    = Front
    | Back
    | Before Int
    | After Int




map : (a -> b) -> STree m a -> STree m b
map f = andThen (One << f)

andThen : (a -> STree m b) -> STree m a -> STree m b
andThen f t =
    case t of
        Zero -> Zero
        One v -> f v
        Many v vs -> Many v <| List.map (andThen f) vs


insert : Int -> a -> STree m a -> Maybe (STree m a)
insert idx v t =
    case t of
        Many vv vs -> Just <| Many vv <| List.take idx vs ++ (One v :: List.drop idx vs)
        _ -> Nothing

{-| Append `v` to the end of the tree level
-}
conj : m -> a -> STree m a -> STree m a
conj m v t =
    case t of
        Zero -> One v
        One vv -> Many m [One vv, One v]
        Many vv vs -> Many vv <| vs ++[One v]

{-| Prepend `v` in front of the tree level
-}
cons : m -> a -> STree m a -> STree m a
cons m v t =
    case t of
        Zero -> One v
        One vv -> Many m [One v, One vv]
        Many vv vs -> Many vv <| (One v) :: vs
-------------------------------------------------------------------------

type Error
    = CannotFindNodeAtCursor Cursor
    | InvalidNodeIndex Cursor Int


type alias Cursor = List Int

cannotFindNodeAt = Err << CannotFindNodeAtCursor
nothingToInvalidNodeIndex i c = Result.fromMaybe (InvalidNodeIndex c i)

at : Cursor -> STree m v -> Result Error (STree m v)
at c t =
    case (c,t) of
        ([], _) -> Ok t
        (i::xs, Many vv vs) ->
            List.Extra.getAt i vs
                |> (nothingToInvalidNodeIndex i c)
                |> Result.andThen (at xs)
        _ -> cannotFindNodeAt c


attemptUpdateAt : (STree m v -> Result Error (STree m v)) -> Cursor -> STree m v -> Result Error (STree m v)
attemptUpdateAt f c t =
    case (c,t) of
        (i::xs, Many vv vs) ->
            List.Extra.getAt i vs
                |> nothingToInvalidNodeIndex i c
                |> Result.andThen (attemptUpdateAt f xs)
                |> Result.andThen (\t -> List.Extra.setAt i t vs |> nothingToInvalidNodeIndex i c)
                |> Result.map (Many vv)
        ([], _) -> f t
        _ -> cannotFindNodeAt c



updateAt : (STree m v -> STree m v) -> Cursor -> STree m v -> Result Error (STree m v)
updateAt f = attemptUpdateAt (Ok << f)

andThenAt : (v -> STree m v) -> Cursor -> STree m v -> Result Error (STree m v)
andThenAt = updateAt << andThen

mapAt : (v -> v) -> Cursor -> STree m v -> Result Error (STree m v)
mapAt = updateAt << map


-------------------------------------------------------------------------

type TransformError x
    = InnerError x
    | OuterError Error


{-| Try to update the target node with a side effect that bubbles up.
-}
attemptUpdateAtWithEffects : (STree m a -> Result x (STree m a,b)) -> Cursor -> STree m a -> Result (TransformError x) (STree m a, b)
attemptUpdateAtWithEffects f c t =
    let
        indexErr h c m =
                nothingToInvalidNodeIndex h c m
                    |> Result.mapError OuterError

        backUp oldManyValue cursorHead childList (newChild, sideEffect)=
                List.Extra.setAt cursorHead newChild childList
                    |> indexErr cursorHead c
                    |> Result.map (\vv -> (Many oldManyValue vv, sideEffect))

        stepInto oldManyValue cursorHead cursorRest childList =
            List.Extra.getAt cursorHead childList
                |> indexErr cursorHead c
                |> Result.andThen (attemptUpdateAtWithEffects f cursorRest)
                |> Result.andThen (backUp oldManyValue cursorHead childList)


    in case (c,t) of
        (i::xs, Many vv vs) -> stepInto vv i xs vs
        ([], _) -> Result.mapError InnerError <| f t
        _ -> Err <| OuterError <| CannotFindNodeAtCursor c


{-| Try to update the target node with a side effect that bubbles up.
-}
updateAtWithEffects : (STree m a -> (STree m a,b)) -> Cursor -> STree m a -> Result (TransformError x) (STree m a, b)
updateAtWithEffects f = attemptUpdateAtWithEffects (Ok << f)
--


type SideEffectErrors
    = CannotTransformManyWithSideEffects Cursor

--
andThenAtWithEffects : (a -> (STree m a, b)) -> Cursor -> STree m a -> Result (TransformError SideEffectErrors) (STree m a,b)
andThenAtWithEffects f c t =
    let fn t =
            case t of
                One v -> Ok <| f v
                _ -> Err <| CannotTransformManyWithSideEffects c
    in attemptUpdateAtWithEffects fn c t


mapAtWithEffects : (a -> (a,b)) -> Cursor -> STree m a -> Result (TransformError SideEffectErrors) (STree m a, b)
mapAtWithEffects f = andThenAtWithEffects (\a -> f a |> Tuple.mapFirst One)

-------------------------------------------------------------------------


insertAt : Cursor -> m -> v -> STree m v -> Result Error (STree m v)
insertAt c m v t =
    if List.isEmpty c then
        Ok <| conj m v t
    else
        let (parentC, idx) = List.Extra.splitAt ((List.length c) - 2) c
        in
            case idx of
                [x] ->
                    attemptUpdateAt (\a -> insert x v a |> Result.fromMaybe (InvalidNodeIndex c x)) parentC t
                _ ->
                    Err <| CannotFindNodeAtCursor c

-------------------------------------------------------------------------



foldAsTree : (a-> b -> b) -> (List b -> b -> b) -> b -> STree m a -> b
foldAsTree oneF manyF b t =
    case t of
        Zero -> b
        One v -> oneF v b
        Many vv vs -> manyF (List.map (foldAsTree oneF manyF b) vs) b


mapAsTree : (a -> b) -> (List b -> b) -> STree m a -> b
mapAsTree oneF manyF t =
    case t of
        Zero -> manyF []
        One v ->  oneF v
        Many vv vs ->  manyF (List.map (mapAsTree oneF manyF) vs)


{-| Same as `mapAsTree`, but also adds a cursor pointing to the current node to the
arguments of the mapper functions
-}
indexedMapAsTree : (Cursor -> a -> b) -> (Cursor -> m -> List b -> b) -> b -> STree m a -> b
indexedMapAsTree oneF manyF init t =
    let recur cursor init t =
            case t of
                Zero -> init
                One v -> oneF cursor v
                Many vv vs ->
                    List.indexedMap (\i -> recur (cursor ++ [i]) init) vs
                        |> manyF cursor vv
    in recur [] init t


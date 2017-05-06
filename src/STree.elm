module STree exposing (..)
{-| Describe me please...
-}

import List.Extra

type STree v
    = Zero
    | One v
    | Many (List (STree v))

empty = Zero


list = Many
concat = Many
leaf = One


fromList : List v -> STree v
fromList = Many << List.map One

-------------------------------------------------------------------------

type InsertPos
    = Front
    | Back
    | Before Int
    | After Int




map : (a -> b) -> STree a -> STree b
map f = andThen (One << f)

andThen : (a -> STree b) -> STree a -> STree b
andThen f t =
    case t of
        Zero -> Zero
        One v -> f v
        Many vs -> Many <| List.map (andThen f) vs


insert : Int -> a -> STree a -> Maybe (STree a)
insert idx v t =
    case t of
        Many vs -> Just <| Many <| List.take idx vs ++ (One v :: List.drop idx vs)
        _ -> Nothing

{-| Append `v` to the end of the tree level
-}
conj : a -> STree a -> STree a
conj v t =
    case t of
        Zero -> One v
        One vv -> Many [One vv, One v]
        Many vs -> Many <| vs ++[One v]

{-| Prepend `v` in front of the tree level
-}
cons : a -> STree a -> STree a
cons v t =
    case t of
        Zero -> One v
        One vv -> Many [One v, One vv]
        Many vs -> Many <| (One v) :: vs
-------------------------------------------------------------------------

type Error
    = CannotFindNodeAtCursor Cursor
    | InvalidNodeIndex Cursor Int


type alias Cursor = List Int

cannotFindNodeAt = Err << CannotFindNodeAtCursor
nothingToInvalidNodeIndex i c = Result.fromMaybe (InvalidNodeIndex c i)

at : Cursor -> STree v -> Result Error (STree v)
at c t =
    case (c,t) of
        ([], _) -> Ok t
        (i::xs, Many vs) ->
            List.Extra.getAt i vs
                |> (nothingToInvalidNodeIndex i c)
                |> Result.andThen (at xs)
        _ -> cannotFindNodeAt c


attemptUpdateAt : (STree v -> Result Error (STree v)) -> Cursor -> STree v -> Result Error (STree v)
attemptUpdateAt f c t =
    case (c,t) of
        (i::xs, Many vs) ->
            List.Extra.getAt i vs
                |> nothingToInvalidNodeIndex i c
                |> Result.andThen (attemptUpdateAt f xs)
                |> Result.andThen (\t -> List.Extra.setAt i t vs |> nothingToInvalidNodeIndex i c)
                |> Result.map Many
        ([], _) -> f t
        _ -> cannotFindNodeAt c



updateAt : (STree v -> STree v) -> Cursor -> STree v -> Result Error (STree v)
updateAt f = attemptUpdateAt (Ok << f)

andThenAt : (v -> STree v) -> Cursor -> STree v -> Result Error (STree v)
andThenAt = updateAt << andThen

mapAt : (v -> v) -> Cursor -> STree v -> Result Error (STree v)
mapAt = updateAt << map


-------------------------------------------------------------------------

type TransformError x
    = InnerError x
    | OuterError Error


{-| Try to update the target node with a side effect that bubbles up.
-}
attemptUpdateAtWithEffects : (STree a -> Result x (STree a,b)) -> Cursor -> STree a -> Result (TransformError x) (STree a, b)
attemptUpdateAtWithEffects f c t =
    let
        indexErr h c m =
                nothingToInvalidNodeIndex h c m
                    |> Result.mapError OuterError

        backUp cursorHead childList (newChild, sideEffect)=
                List.Extra.setAt cursorHead newChild childList
                    |> indexErr cursorHead c
                    |> Result.map (\vv -> (Many vv, sideEffect))

        stepInto cursorHead cursorRest childList =
            List.Extra.getAt cursorHead childList
                |> indexErr cursorHead c
                |> Result.andThen (attemptUpdateAtWithEffects f cursorRest)
                |> Result.andThen (backUp cursorHead childList)


    in case (c,t) of
        (i::xs, Many vs) -> stepInto i xs vs
        ([], _) -> Result.mapError InnerError <| f t
        _ -> Err <| OuterError <| CannotFindNodeAtCursor c


{-| Try to update the target node with a side effect that bubbles up.
-}
updateAtWithEffects : (STree a -> (STree a,b)) -> Cursor -> STree a -> Result (TransformError x) (STree a, b)
updateAtWithEffects f = attemptUpdateAtWithEffects (Ok << f)
--


type SideEffectErrors
    = CannotTransformManyWithSideEffects Cursor

--
andThenAtWithEffects : (a -> (STree a, b)) -> Cursor -> STree a -> Result (TransformError SideEffectErrors) (STree a,b)
andThenAtWithEffects f c t =
    let fn t =
            case t of
                One v -> Ok <| f v
                _ -> Err <| CannotTransformManyWithSideEffects c
    in attemptUpdateAtWithEffects fn c t


mapAtWithEffects : (a -> (a,b)) -> Cursor -> STree a -> Result (TransformError SideEffectErrors) (STree a, b)
mapAtWithEffects f = andThenAtWithEffects (\a -> f a |> Tuple.mapFirst One)

-------------------------------------------------------------------------


insertAt : Cursor -> v -> STree v -> Result Error (STree v)
insertAt c v t =
    if List.isEmpty c then
        Ok <| conj v t
    else
        let (parentC, idx) = List.Extra.splitAt ((List.length c) - 2) c
        in
            case idx of
                [x] ->
                    attemptUpdateAt (\a -> insert x v a |> Result.fromMaybe (InvalidNodeIndex c x)) parentC t
                _ ->
                    Err <| CannotFindNodeAtCursor c

-------------------------------------------------------------------------



foldAsTree : (a-> b -> b) -> (List b -> b -> b) -> b -> STree a -> b
foldAsTree oneF manyF b t =
    case t of
        Zero -> b
        One v -> oneF v b
        Many vs -> manyF (List.map (foldAsTree oneF manyF b) vs) b


mapAsTree : (a -> b) -> (List b -> b) -> STree a -> b
mapAsTree oneF manyF t =
    case t of
        Zero -> manyF []
        One v ->  oneF v
        Many vs ->  manyF (List.map (mapAsTree oneF manyF) vs)


{-| Same as `mapAsTree`, but also adds a cursor pointing to the current node to the
arguments of the mapper functions
-}
indexedMapAsTree : (Cursor -> a -> b) -> (Cursor -> List b -> b) -> STree a -> b
indexedMapAsTree oneF manyF t =
    let recur cursor t =
            case t of
                Zero -> manyF cursor []
                One v -> oneF cursor v
                Many vs ->
                    List.indexedMap (\i -> recur (cursor ++ [i])) vs
                        |> manyF cursor
    in recur [] t

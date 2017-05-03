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


attemptTransformAt : (STree v -> Result Error (STree v)) -> Cursor -> STree v -> Result Error (STree v)
attemptTransformAt f c t =
    case (c,t) of
        (i::xs, Many vs) ->
            List.Extra.getAt i vs
                |> nothingToInvalidNodeIndex i c
                |> Result.andThen (attemptTransformAt f xs)
                |> Result.andThen (\t -> List.Extra.setAt i t vs |> nothingToInvalidNodeIndex i c)
                |> Result.map Many
        ([], _) -> f t
        _ -> cannotFindNodeAt c



transformAt : (STree v -> STree v) -> Cursor -> STree v -> Result Error (STree v)
transformAt f = attemptTransformAt (Ok << f)

andThenAt : (v -> STree v) -> Cursor -> STree v -> Result Error (STree v)
andThenAt = transformAt << andThen

mapAt : (v -> v) -> Cursor -> STree v -> Result Error (STree v)
mapAt = transformAt << map


-------------------------------------------------------------------------

type TransformError x
    = InnerError x
    | OuterError Error


{-| Try to update the target node with a side effect that bubbles up.
-}
attemptTransformAtWithEffects : (STree a -> Result x (STree a,b)) -> Cursor -> STree a -> Result (TransformError x) (STree a, b)
attemptTransformAtWithEffects f c t =
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
                |> Result.andThen (attemptTransformAtWithEffects f cursorRest)
                |> Result.andThen (backUp cursorHead childList)


    in case (c,t) of
        (i::xs, Many vs) -> stepInto i xs vs
        ([], _) -> Result.mapError InnerError <| f t
        _ -> Err <| OuterError <| CannotFindNodeAtCursor c


{-| Try to update the target node with a side effect that bubbles up.
-}
transformAtWithEffects : (STree a -> (STree a,b)) -> Cursor -> STree a -> Result (TransformError x) (STree a, b)
transformAtWithEffects f = attemptTransformAtWithEffects (Ok << f)
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
    in attemptTransformAtWithEffects fn c t


mapWithEffects : (a -> (a,b)) -> Cursor -> STree a -> Result (TransformError SideEffectErrors) (STree a, b)
mapWithEffects f = andThenAtWithEffects (\a -> f a |> Tuple.mapFirst One)

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
                    attemptTransformAt (\a -> insert x v a |> Result.fromMaybe (InvalidNodeIndex c x)) parentC t
                _ ->
                    Err <| CannotFindNodeAtCursor c


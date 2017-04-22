module Bsp.Cursor
    exposing
        ( Cursor(..)
        , CursorFn
        , parentCursor
        , BspStep(..), BStepFn, StepFn
        , foldCursor
        , at
        )

{-| Describe me please...
-}

-- CURSOR AND CURSOR OPERATIONS ------------------------------------------------


type BspStep
    = Left
    | Right


type CursorBase s
    = Step s
    | Head


type Cursor
    = CLeft Cursor
    | CRight Cursor
    | CHead


type alias CursorFn =
    Cursor -> Cursor


parentCursor : Cursor -> Maybe Cursor
parentCursor c =
    case c of
        CLeft CHead ->
            Just CHead

        CRight CHead ->
            Just CHead

        CLeft cc ->
            parentCursor cc |> Maybe.map CLeft

        CRight cc ->
            parentCursor cc |> Maybe.map CRight

        _ ->
            Nothing


at : BStepFn x v -> Cursor -> v -> Result x v
at into c v =
    let
        stepInto cc ( vv, _ ) =
            at into cc vv

        stepIntoDirection dir cc =
            into dir v |> Result.andThen (stepInto cc)
    in
        case c of
            CHead ->
                Ok v

            CLeft cc ->
                stepIntoDirection Left cc

            CRight cc ->
                stepIntoDirection Right cc


type alias StepFn x s v =
    s -> v -> Result x ( v, v -> v )


type alias BStepFn x v =
    StepFn x BspStep v


foldCursor : (v -> Result x v) -> BStepFn x v -> v -> Cursor -> Result x v
foldCursor head into v c =
    let
        stepInto cc ( vv, make ) =
            foldCursor head into vv cc |> Result.map make

        stepIntoDirection dir cc =
            into dir v |> Result.andThen (stepInto cc)
    in
        case c of
            CHead ->
                head v

            CLeft cc ->
                stepIntoDirection Left cc

            CRight cc ->
                stepIntoDirection Right cc

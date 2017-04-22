module Bsp.Error exposing (..)
{-| Describe me please...
-}
import Bsp.Indented as Indented exposing (Line(..), applyIndents, indentWith)

type Error
    = Error { msg : String , children: List Error }


withMsg : List String -> String
withMsg ss = (String.join " " ss)

{-| Create a result from a string.
-}
err: String -> Result Error a
err s = Err <| make s

{-| Create a result from a string list.
-}
errMsg: List String -> Result Error a
errMsg = err << withMsg


{-| Blank
-}
empty : Error
empty = Error { msg = "", children = [] }


{-| Create an error from a string.
-}
make : String -> Error
make s = Error { msg = s, children = [] }

{-| Create an error from a string list.
-}
makeMsg : List String -> Error
makeMsg = make << withMsg


-- BASE WRAP

{-| Returns a new Error wrapping e
-}
wrap : String -> Error -> Error
wrap s e = Error { msg = s, children = [e] }

{-| Returns a new Error wrapping e
-}
wrapMsg : List String -> Error -> Error
wrapMsg = wrap << withMsg

{-| Returns a new Error wrapping the errors
-}
wrapIn : String -> List Error -> Error
wrapIn s es = Error { msg = s, children = es }

{-| Returns a new Error wrapping the errors
-}
wrapInMsg : List String -> List Error -> Error
wrapInMsg = wrapIn << withMsg

{-| Wraps the potential Error in a result
-}
wrapError : String -> Result Error a -> Result Error a
wrapError s r =
    Result.mapError (\e -> wrapIn s [e]) r


{-| Wraps the potential Error in a result with a list argument
-}
wrapErrorMsg : List String -> Result Error a -> Result Error a
wrapErrorMsg = wrapError << withMsg

{-| Converts a list of potential errors to a list of inners or an error if any are Err
-}
wrapErrors : String -> List (Result Error a) -> Result Error (List a)
wrapErrors s rs =
    let
        folder r m =
            case (r, m) of
                (Ok oo, Ok mo) -> Ok (oo :: mo)
                (Ok oo, Err mo) -> m
                (Err oo, Ok mo) -> Err [oo]
                (Err oo, Err mo) -> Err (oo :: mo)

    in
        List.foldr folder (Ok []) rs
            |> Result.mapError (wrapIn s)


errorToString : Error -> String
errorToString e =
    toStringHelper e
        |> indentWith "  "

toStringHelper e =
    case e of
        Error { msg, children } ->
            List.concat
                [ [Text msg, Indent]
                , List.concatMap toStringHelper children
                , [Outdent]
                ]

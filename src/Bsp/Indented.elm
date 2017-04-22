module Bsp.Indented exposing
    ( Line(..), Token, TextTable

    , applyIndents, indentWith

    , concatLine, concatStatement

    , line)

import List.Extra


type Line
    = Text String
    | Indent
    | Outdent
    | Empty
    | Table TextTable

type alias Token =
    { class: String
    , tag: String
    , text: String
    }

{-| A list of rows where columns of tokens will be left-aligned
|-}
type alias TextTable = List (List Token)



mapJoin : String -> (a -> String) -> List a -> String
mapJoin joiner f lst =
    List.map f lst |> String.join joiner

{-| Outputs a single line of text
|-}
line : List String -> Line
line ss = Text <| String.join " " <| ss


{-
   Creates a list of lines from individual tokens
-}


concatLine : List String -> List Line
concatLine bits =
    [ Text (String.join " " bits) ]


concatStatement : List String -> List Line
concatStatement bits =
    [ Text ((String.join " " bits) ++ ";") ]


type alias AppendLineIndentState =
    { indent : Int, lines : List String }


applyLineIndent : String -> Line -> AppendLineIndentState -> AppendLineIndentState
applyLineIndent indentStr line state =
    let
        withIndent s = (String.repeat state.indent indentStr) ++ s
        withNewLine s =
            state.lines ++ [ withIndent s ]
    in
        case line of
            Text s ->
                { state | lines = withNewLine s }

            Indent ->
                { state | indent = state.indent + 1 }

            Outdent ->
                { state | indent = max (state.indent - 1) 0 }

            Empty ->
                { state | lines = state.lines ++ [""] }

            Table t ->
                { state | lines = (++) state.lines <| List.map withIndent <| wrapTable "  " t }


{-| The length of a tokens text
|-}
tokenLen : Token -> Int
tokenLen = (String.length << .text)

{-| Make a token a right-padded string
|-}
rPadToken : Int -> Token -> String
rPadToken w s = (++) s.text <|String.repeat (w - tokenLen s) " "


applyList : List (a -> b) -> List a -> List b
applyList fns es =
    List.map2 (<|) fns es

wrapTable : String -> TextTable -> List String
wrapTable separator t =
    let
        sep = if List.length t == 1 then " " else separator
        colSize c = Maybe.withDefault 0 <| List.maximum <| List.map tokenLen c

        cols = List.Extra.transpose t |> List.map colSize

        extendCols = List.map rPadToken cols
        cs = List.map (String.join sep << applyList extendCols) t


    in
        cs

{-
   Applies the indentation guides to the text
-}


applyIndents : List Line -> List String
applyIndents lines =
    let
        init =
            { lines = [], indent = 0 }

        indentedLines =
            List.foldl (applyLineIndent "\t") init lines
    in
        indentedLines.lines

{-| Applies the given indentation string and concatenates the results
-}
indentWith : String -> List Line -> String
indentWith indentStr lines =
    let
        init =
            { lines = [], indent = 0 }

        indentedLines =
            List.foldl (applyLineIndent indentStr) init lines
    in
        indentedLines.lines |> String.join "\n"

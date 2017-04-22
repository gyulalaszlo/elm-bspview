module Bsp.Test exposing (main)
{-| Describe me please...
-}
import Bsp.Cursor exposing (parentCursor)
import Bsp.DefaultTheme
import Bsp.Msg exposing (Msg(..), LayoutEditingMode(..))
import Bsp.Traits exposing (LocalModel, SharedModel)
import Bsp.Model exposing (Model, modelFrom)
import Bsp.Root exposing (subscriptions, update, view)
import Bsp.SplitView exposing (Direction(Horizontal, Vertical), RotateDirection(CCW, CW))
import Colors.Monokai
import Bsp.Css as Css
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task

type alias Msg = Bsp.Msg.Msg ChildMsg ChildModel
type alias Model = LocalModel ChildMsg ChildModel Int
--type alias Model = Bsp.Root.Model ChildMsg ChildModel Int

--init : ( Model, Cmd Msg )



init =
    ( modelFrom bspTraits 0, Cmd.none )
    -- ( initialModel, Task.perform INIT_MSG (Task.succeed MSG_DATA) )


withCss : String -> (model -> Html msg) -> model -> Html msg
withCss css view model =
    Html.div []
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css"]
            [ Html.text css ]

        , view model
        ]


--main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = withCss Bsp.DefaultTheme.css view
        , update = update
        , subscriptions = subscriptions
        }

type ChildMsg = Noop

type ChildModel
    = A
    | B

bspTraits =
    { subscriptions = always Sub.none
    , update = childUpdate
    , view = childView

    , wrapper = Bsp.DefaultTheme.normalTheme childLabel (\_ -> [A,B])
    }


-- VIEW: childView


childUpdate : ChildMsg -> Model -> (ChildModel, Int, Cmd Msg)
childUpdate msg  model =
    let {local,shared} = model
    in case local of
        A -> (local, shared, Cmd.none)
        B -> (local, shared, Cmd.none)


{-| child view
-}
childView : Model -> Html Msg
childView {local, shared, cursor, msg} =

    Html.div []
        [ Html.h4 [] [ text <| childLabel local]
        , case local of
            A ->
                Html.div []
                    [ Html.pre [] [ text Bsp.DefaultTheme.css ]
                    ]

            B ->
                Html.div []
                    [ Html.text  "BBBB"
                    ]

        ]





childLabel : ChildModel -> String
childLabel m =
    case m of
        A -> "CSS View"
        B -> "Blank"

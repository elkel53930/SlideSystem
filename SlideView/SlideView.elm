module SlideView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
import Task
import Mouse
import Keyboard
import Char
import String
import Markdown

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { now : Time
  , start : Time
  , clickCount : Int
  , string : String
  }

type alias Page = Html Msg

slide : Page
slide = Markdown.toHtml [] """
# これはマークダウンで記述されたスライドです

hogehoge

"""


init : (Model, Cmd Msg)
init =
  ( Model 0 0 0 ""
  , Task.perform Init Time.now
  )




-- UPDATE


type Msg
  = Tick Time
  | Init Time
  | Click Mouse.Position
  | Key Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model
        | now=newTime}
      , Cmd.none
      )

    Init startTime ->
      ( { model
        | now = startTime
        , start = startTime
        }
      , Cmd.none
      )

    Click pos ->
      ( { model
        | clickCount = model.clickCount + 1
        }
      , Cmd.none
      )

    Key code ->
    ( { model
      | string = model.string ++ ( code |> Char.fromCode |> String.fromChar )
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Mouse.clicks Click
    , Keyboard.downs Key
    , Time.every second Tick
    ]




-- VIEW


view : Model -> Html Msg
view model =
  Html.div []
      [ Html.text
        ( Time.inSeconds (model.now - model.start)
          |> round
          |> toString
        )
      , Html.text
        ( toString model.clickCount )
      , Html.text model.string
      , slide
      , footer model
      ]

footer : Model -> Html Msg
footer model =
    Html.footer [ class "info" ]
        [ Html.small [] [ Html.text "Slide" ]
        ]

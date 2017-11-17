module SlideView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
import Task
import Mouse
import Keyboard
import Markdown

index : List a -> Int -> Maybe a
index xs n  = List.head (List.drop n xs)

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
  , pageNum : Int
  , lastKeyCode : Keyboard.KeyCode
  }

type alias Page =
  { title : String
  , html : Html Msg
  }

slide : List Page
slide =
  [ Page "Page1" (Markdown.toHtml [] """
# これはマークダウンで記述されたスライドです

hogehoge

""")

  , Page "Page 2" (Markdown.toHtml [] """
# 2ページ目

fugafuga
""")

  , Page "Page 3" (Markdown.toHtml [] """
# 3ページ目

piyopiyo

""")

  , Page "Last page" (Markdown.toHtml [] """

# 4ページ目

42

""")]

totalPageNum : Int
totalPageNum = List.length slide


init : (Model, Cmd Msg)
init =
  ( Model 0 0 0 0 0
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
    ( pageUpdateByKey model code
    , Cmd.none
    )

pageUpdateByKey : Model -> Keyboard.KeyCode -> Model
pageUpdateByKey model code =
  case code of
    37 -> pageUpdate model -1 -- Left key
    39 -> pageUpdate model  1 -- Right key
    _  -> model

pageUpdate : Model -> Int -> Model
pageUpdate model add =
  { model
  | pageNum = (model.pageNum + add) % totalPageNum
  }

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
      [ header model
      , contents model
      , footer model
      ]

contents : Model -> Html Msg
contents model =
  case index slide model.pageNum of
    Just html -> div [ id "contents"] [html.html]
    Nothing   -> div [ id "contents"] [Markdown.toHtml [] "# No page data!!"]

header : Model -> Html Msg
header model =
  Html.header [ id "header" ]
    [ div [] [ Html.h1 [] [Html.text
      ( case index slide model.pageNum of
          Just html -> html.title
          Nothing   -> "")] ]
    ]

footer : Model -> Html Msg
footer model =
  Html.footer [ id "footer" ]
    [ div [] [ Html.text ("Slide (" ++ toString (model.pageNum+1) ++ "/" ++ toString totalPageNum ++ ")") ]
    , div [] [ Html.text (  Time.inSeconds (model.now - model.start)
                                         |> round
                                         |> secToString ) ]
    ]

secToString : Int -> String
secToString time =  toString (time // 60)
                 ++ "min. "
                 ++ toString (time % 60)
                 ++ "sec."

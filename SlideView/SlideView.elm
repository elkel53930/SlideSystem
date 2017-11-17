module SlideView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
import Task
import Mouse
import Keyboard
import Markdown
{-
keyLeft : Keyboard.KeyCode
keyLeft = 37
keyRight : Keyboard.KeyCode
keyRight = 39
keyUp : Keyboard.KeyCode
keyUp = 38
keyDown : Keyboard.KeyCode
keyDown = 40
-}
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

type alias Page = Html Msg

slide : List Page
slide = [ Markdown.toHtml [] """
# これはマークダウンで記述されたスライドです

hogehoge

"""

  ,Markdown.toHtml [] """
# 2ページ目

fugafuga
"""

  ,Markdown.toHtml [] """
# 3ページ目

piyopiyo

"""

  ,Markdown.toHtml [] """

# 4ページ目

42

"""]

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
    37  -> pageUpdate model -1
    39 -> pageUpdate model 1
    38    -> pageUpdate model -3
    40  -> pageUpdate model 3
    _        -> model

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
      [ Html.text
        ( Time.inSeconds (model.now - model.start)
          |> round
          |> toString
        )
      , Html.text
        ( toString model.clickCount )
      , Html.text (toString model.lastKeyCode)
      , getPage model
      , footer model
      ]

getPage : Model -> Html Msg
getPage model =
  case index slide model.pageNum of
    Just html -> html
    Nothing   -> Markdown.toHtml [] "# No page data!!"

footer : Model -> Html Msg
footer model =
    Html.footer [ class "footer" ]
        [ div [] [ Html.small [] [ Html.text ("Slide (" ++ toString model.pageNum ++ "/" ++ toString totalPageNum ++ ")") ] ]
        , div [] [ Html.small [] [ Html.text (  Time.inSeconds (model.now - model.start)
                                    |> round
                                    |> secToString ) ] ]
        ]

secToString : Int -> String
secToString time =  toString (time // 60)
                 ++ "min. "
                 ++ toString (time % 60)
                 ++ "sec."

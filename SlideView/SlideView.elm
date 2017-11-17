module SlideView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
import Task
import Mouse
import Keyboard
import Markdown
import Css

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
  [ Page "" ( div []
              [ h1  [ style [ ("text-align","center") ] ] [ text "プライベートでの開発" ]
              , div [ style [ ("text-align","center") ] ] [ text "17.11.20 飯田一輝"] ]
            )
  , Page "ToC" (Markdown.toHtml [] """
* 自己紹介
* マイクロマウス
* 回路記述言語 Eisler
* プレゼンテーションアプリ
""")

  , Page "Page 2" (Markdown.toHtml [] """
# CSSの勉強メンドクサイ

**Strong**
*Italic*

* **文字列を強調**することもできます。

```
#include <stdio.h>

void main()
{
  printf("Hello,World!\\n");
  return;
}
```
""")

  , Page "Page 3" (Markdown.toHtml [] """
# This is 3rd page.

* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A
* A


""")

  , Page "Last page" (Markdown.toHtml [] """

# Last!

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

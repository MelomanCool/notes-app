import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (decodeString, list, errorToString)
import Json.Encode exposing (encode)
import Markdown

import Json exposing (decodeTag, encodeTag, decodeNote, encodeNote)
import Types exposing (Tag(..), Note(..))


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = (\_ -> Sub.none)
    , view = view
    }


type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "http://localhost:6868/notes"
      , expect = Http.expectString GotText
      }
  )


type Msg
  = GotText (Result Http.Error String)
  | UpdatedNote String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

    UpdatedNote noteString ->
      (Success noteString, Cmd.none)


renderedPanel : Note -> Element Msg
renderedPanel (Note note) =
  column
    [ height fill
    , width fill
    , padding 20
    ]
    [ paragraph []
        [ html (Markdown.toHtml [] note.text) ]
    ]

editPanel : Note -> Element Msg
editPanel (Note note) =
    column [ height fill
           , width fill
           ]
        [ (Input.multiline [ height fill, Border.rounded 0 ]
                    { label = Input.labelHidden ""
                    , onChange = (\x -> Note { note | text = x}
                                 |> encodeNote
                                 |> encode 0
                                 |> (\y -> "[" ++ y ++ "]")
                                 |> UpdatedNote)
                    , placeholder = Nothing
                    , spellcheck = False
                    , text = note.text
                    })
        ]


view : Model -> Html Msg
view model =
  layout [] <|
    case model of
      Failure ->
        text "I was unable to load your book."

      Loading ->
        text "Loading..."

      Success fullText ->
        case decodeString (list decodeNote) fullText of
          Ok notes ->
            row [ height fill, width fill ]
                [ editPanel
                    (notes
                    |> List.head
                    |> Maybe.withDefault
                       (Note { id = 0
                             , tags = []
                             , text = ""
                             , title = ""}))
                , renderedPanel
                    (notes
                    |> List.head
                    |> Maybe.withDefault
                       (Note { id = 0
                             , tags = []
                             , text = ""
                             , title = ""}))
                ]

          Err error ->
            text <| errorToString error

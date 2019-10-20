import Browser
import Html exposing (Html, text, div, table, thead, th, tr, td, pre)
import Http
import Json.Decode exposing (decodeString, list, errorToString)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


viewNote : Note -> Html Msg
viewNote (Note note) =
  table []
    [ thead []
        [ th [] [ text note.title ]]
    , tr []
      [ td [] (Markdown.toHtml Nothing note.text) ]
    ]

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      case decodeString (list decodeNote) fullText of
        Ok notes ->
          div [] (List.map viewNote notes)
            
        Err error ->
          text <| errorToString error

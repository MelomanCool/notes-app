module Json exposing (decodeTag, encodeTag, decodeNote, encodeNote)

import Json.Encode as E
import Json.Decode as D

import Types exposing (Tag(..), Note(..))


decodeTag : D.Decoder Tag
decodeTag =
    D.map2 (\ id name -> Tag { id = id, name = name })
      (D.field "id" D.int)
      (D.field "name" D.string)

encodeTag : Tag -> E.Value
encodeTag (Tag record) =
    E.object
        [ ("id",  E.int <| record.id)
        , ("name",  E.string <| record.name)
        ]


decodeNote : D.Decoder Note
decodeNote =
  D.map5 (\ id title text textFormat tags
         -> Note { id = id, title = title, text = text, textFormat = textFormat, tags = tags } )
    (D.field "id" D.int)
    (D.field "title" D.string)
    (D.field "text" D.string)
    (D.field "textFormat" D.string)
    (D.field "tags" <| D.list decodeTag )

encodeNote : Note -> E.Value
encodeNote (Note record) =
    E.object
        [ ("id",  E.int <| record.id)
        , ("title",  E.string <| record.title)
        , ("text",  E.string <| record.text)
        , ("textFormat",  E.string <| record.textFormat)
        , ("tags",  E.list encodeTag record.tags)
        ]

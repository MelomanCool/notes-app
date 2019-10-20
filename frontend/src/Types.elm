module Types exposing (Tag(..), Note(..))

type Tag = Tag { id : Int
               , name : String
               }

type Note = Note { id : Int
                 , title : String
                 , text : String
                 , textFormat : String
                 , tags : List Tag
                 }

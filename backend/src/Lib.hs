{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( webAppEntry
    , Tag
    , Note
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant (serve, Proxy(..), Server, JSON, Get, (:>), Handler, throwError, err404)


data Tag = Tag { id :: Int
               , name :: String
               } deriving (Generic, Show)

instance ToJSON Tag
instance FromJSON Tag


data Note = Note { id :: Int
                 , title :: String
                 , text :: String
                 , tags :: [Tag]
                 } deriving (Generic, Show)

instance ToJSON Note
instance FromJSON Note


loadNotes :: IO (Maybe [Note])
loadNotes = fmap decode (B.readFile "notes.json")

notesHandler :: Handler [Note]
notesHandler = do
  mNotes <- liftIO $ loadNotes
  case mNotes of
    Just notes -> return notes
    Nothing    -> throwError err404


type NoteAPI = "notes" :> Get '[JSON] [Note]

server :: Server NoteAPI
server = notesHandler

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

webAppEntry :: IO ()
webAppEntry = run 6868 $ simpleCors $ serve noteAPI server

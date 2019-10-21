{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( webAppEntry
    , Tag
    , Note
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy.UTF8 as BLU
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant (serve, Proxy(Proxy), Server, JSON, Get, (:>), Handler, throwError, err500, errBody)


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


eitherReadFile :: FilePath -> IO (Either String ByteString)
eitherReadFile p =
  (Right <$> B.readFile p) `catch`
  ((\e -> pure (Left $ show e)) :: IOException -> IO (Either String ByteString))


loadNotes :: IO (Either String [Note])
loadNotes = fmap (>>= eitherDecode) (eitherReadFile "notes.json")

notesHandler :: Handler [Note]
notesHandler = do
  mNotes <- liftIO $ loadNotes
  case mNotes of
    Right notes -> return notes
    Left error  -> throwError err500 { errBody = BLU.fromString error }


type NoteAPI = "notes" :> Get '[JSON] [Note]

server :: Server NoteAPI
server = notesHandler

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

webAppEntry :: IO ()
webAppEntry = run 6868 $ simpleCors $ serve noteAPI server

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( webAppEntry
    , Tag
    , Note
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson(ToJSON, FromJSON)
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant(serve, Proxy(..), Server, JSON, Get, (:>))
import qualified Data.ByteString.Lazy as B


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


notes :: IO [Note]
notes = do
  notesStr <- B.readFile "notes.json"
  return $ fromMaybe [] (decode notesStr :: Maybe [Note])


type NoteAPI = "notes" :> Get '[JSON] [Note]

server :: Server NoteAPI
server = liftIO notes

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

webAppEntry :: IO ()
webAppEntry = run 6868 $ simpleCors $ serve noteAPI server

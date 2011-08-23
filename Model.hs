{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Model where

import Yesod
import Yesod.Auth.HashDB
import Data.Text (Text,unpack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time

data EventType = Class | Talk | Workshop
                 deriving (Show, Read, Eq)
derivePersistField "EventType"

data UserType = CoFounder | Member | Collaborator | Student | Presenter
                deriving (Show, Read, Eq)
derivePersistField "UserType"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll"] $(persistFile "config/models")

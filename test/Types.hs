module Types where

import           Database.MySQL.Base (Connection)
import           Servant.Client      (ClientEnv)

data Env =
  Env
  { dbConn        :: Connection
  , servantClient :: ClientEnv
  , wpUser        :: String
  , wpPassword    :: String
  , reset         :: Connection -> IO ()
  }

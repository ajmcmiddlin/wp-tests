{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Exception     (bracket)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Database.MySQL.Base   (Connection, connect, connectDatabase, close,
                                        connectHost, connectPassword,
                                        connectUser, defaultConnectInfo, query)
import           Network.HTTP.Client   (defaultManagerSettings, newManager)
import           Servant.Client        (BaseUrl (..), ClientEnv (ClientEnv),
                                        Scheme (Http))
import           System.Environment    (getArgs)

import           Types                 (Env (..))

main :: IO ()
main =
  getArgs >>= \case
    (ip:user:pass:resetFile:_) -> runWithArgs ip user pass resetFile
    _ -> error "Must provide ip, WP user, WP pass, and DB reset SQL"

runWithArgs
  :: String
  -> String
  -> String
  -> String
  -> IO ()
runWithArgs ip wpUser wpPassword resetSqlFile = do
  let
    dbConnInfo =
      defaultConnectInfo
      { connectHost = ip
      , connectUser = "wordpress"
      , connectPassword = "wordpress"
      , connectDatabase = "wordpress"
      }
  resetSql <- readFile resetSqlFile
  mgr <- newManager defaultManagerSettings
  let
    servantClient = ClientEnv mgr $ BaseUrl Http ip 80 ""
    reset = resetUsing (pack resetSql)
  bracket
    (connect dbConnInfo)
    close
    (const $ runWithEnv Env{..})

runWithEnv
  :: Env
  -> IO ()
runWithEnv env =
  error "TODO: runWithEnv"

resetUsing
  :: ByteString
  -> Connection
  -> IO ()
resetUsing resetSql conn =
  query conn resetSql

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Data.ByteString.Char8    as BS8
import           Data.Functor             (void)
import           Data.Semigroup           ((<>))
import           Database.MySQL.Base      (connectDatabase, connectHost,
                                           connectPassword, connectUser,
                                           defaultConnectInfo)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (BaseUrl (..), ClientEnv (ClientEnv),
                                           Scheme (Http))
import           System.Environment       (getEnv)
import           System.Process           (readProcess)

import           Test.Tasty               (defaultMain, testGroup)

import           Types                    (Env (..))
import           WordPressTests           (wordpressTests)

main :: IO ()
main =
  mkEnv >>= runWithEnv

mkEnv
  :: IO Env
mkEnv = do
  mgr <- newManager defaultManagerSettings
  host <- getEnv "WP_HOST"
  wpUser <- BS8.pack <$> getEnv "WP_USER"
  wpPassword <- BS8.pack <$> getEnv "WP_PASS"
  resetSqlFile <- getEnv "WP_RESET_PATH"
  let
    dbUser = "wordpress"
    dbPassword = "wordpress"
    dbDatabase = "wordpress"
    dbConnInfo =
      defaultConnectInfo
      { connectHost = host
      , connectUser = dbUser
      , connectPassword = dbPassword
      , connectDatabase = dbDatabase
      }
    dbCmdLineArgs =
      [ "-h", host
      , "-u", dbUser
      , "--password=" <> dbPassword
      , dbDatabase
      ]
    servantClient = ClientEnv mgr $ BaseUrl Http host 80 "/wp-json/wp/v2"
    reset = resetUsing resetSqlFile dbCmdLineArgs
  pure Env{..}

runWithEnv
  :: Env
  -> IO ()
runWithEnv env =
  defaultMain $ testGroup "wordpress"
  [ wordpressTests env
  ]

resetUsing
  :: FilePath
  -> [String]
  -> IO ()
resetUsing resetSql connectionArgs =
  readFile resetSql >>= void . readProcess "mysql" connectionArgs

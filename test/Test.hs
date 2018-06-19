{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Exception        (bracket)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack)
import qualified Data.Dependent.Map       as DM
import           Data.Foldable            (traverse_)
import           Data.Functor             (void)
import           Data.Semigroup           ((<>))
import           Database.MySQL.Base      (Connection, close, connect,
                                           connectDatabase, connectHost,
                                           connectPassword, connectUser,
                                           defaultConnectInfo, query)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (BaseUrl (..), ClientEnv (ClientEnv),
                                           Scheme (Http), ServantError,
                                           runClientM)
import           System.Environment       (getEnv)
import           System.Process           (readProcess)

import           Test.Tasty               (TestTree, defaultMain, testGroup)

import           Types                    (Env (..))
import           Web.WordPress.API        (listPosts)
import           Web.WordPress.Types.Post (PostMap)
import           WordPressTests           (wordpressTests)

main :: IO ()
main = do
  wpHost <- getEnv "WP_HOST"
  wpUser <- getEnv "WP_USER"
  wpPass <- getEnv "WP_PASS"
  wpReset <- getEnv "WP_RESET_PATH"
  runWithArgs wpHost wpUser wpPass wpReset

-- <3 String
runWithArgs
  :: String
  -> String
  -> String
  -> String
  -> IO ()
runWithArgs host wpUser wpPassword resetSqlFile = do
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
  mgr <- newManager defaultManagerSettings
  let
    servantClient = ClientEnv mgr $ BaseUrl Http host 80 "/wp-json/wp/v2"
    reset = resetUsing resetSqlFile dbCmdLineArgs
    lp = DM.empty
  bracket
    (connect dbConnInfo)
    close
    (\dbConn -> runWithEnv Env{..})

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

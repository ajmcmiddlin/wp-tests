{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Exception        (bracket)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS8
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
    lp = DM.empty
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

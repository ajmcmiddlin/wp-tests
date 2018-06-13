{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Exception        (bracket)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack)
import qualified Data.Dependent.Map as DM
import           Database.MySQL.Base      (Connection, close, connect,
                                           connectDatabase, connectHost,
                                           connectPassword, connectUser,
                                           defaultConnectInfo, query)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (BaseUrl (..), ClientEnv (ClientEnv), ServantError,
                                           Scheme (Http), runClientM)
import           System.Environment       (getArgs)

import           Test.Tasty               (TestTree, defaultMain, testGroup)

import           Types                    (Env (..))
import           Web.WordPress.API        (listPosts)
import           Web.WordPress.Types.Post (PostMap)
import           WordPressTests           (wordpressTests)

main :: IO ()
main =
  getArgs >>= \case
    (ip:user:pass:resetFile:_) -> do
      r <- runWithArgs ip user pass resetFile
      case r of
        Left e -> print e
        Right (p:_) -> print p
    _ -> error "Must provide ip, WP user, WP pass, and DB reset SQL"

-- <3 String
runWithArgs
  :: String
  -> String
  -> String
  -> String
  -> IO (Either ServantError [PostMap])
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
    servantClient = ClientEnv mgr $ BaseUrl Http ip 80 "/wp-json/wp/v2"
    reset = resetUsing (pack resetSql)
    lp = DM.empty
  runClientM (listPosts lp) servantClient
  -- bracket
  --   (connect dbConnInfo)
  --   close
  --   (\dbConn -> runWithEnv Env{..})

runWithEnv
  :: Env
  -> IO ()
runWithEnv env =
  defaultMain $ testGroup "wordpress"
  [ wordpressTests env
  ]

resetUsing
  :: ByteString
  -> Connection
  -> IO ()
resetUsing resetSql conn =
  query conn resetSql

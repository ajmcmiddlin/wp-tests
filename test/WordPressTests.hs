module WordPressTests where

import           Hedgehog       (Callback (..), Command (Command),
                                 HTraversable (htraverse), MonadGen, MonadTest,
                                 Var (Var), annotateShow, assert, concrete,
                                 evalEither, failure, success, (===))
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty     (TestTree)

import           Types          (Env)



wordpressTests
  :: Env
  -> TestTree
wordpressTests env =
  error "TODO: wordpressTests"

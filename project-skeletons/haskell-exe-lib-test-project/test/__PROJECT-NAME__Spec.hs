module __PROJECT-NAME__Spec (spec) where

import           Test.Hspec
import           __PROJECT-NAME__

-- `main` is here so that this module can be run from GHCi on its own. It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "behavior" $ do

    it "returns something" $ do
      pendingWith "need to write the first testcase"

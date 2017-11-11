import Test.Hspec
import KotNeuralNetwork
import KotNeuralNetwork.Internal

import qualified SpecFeedforward as SFF

main :: IO()
main = hspec $ do
  describe "Global test:" $ do
    it "Calculating E_total" $ do
      totalError [2, 3] [0, 1] `shouldBe` (4 :: Double)
    it "Calculating E_partial" $ do
      partialErrors [2, 3] [7, 10] `shouldBe` ([5, 7] :: [Double])
  SFF.test

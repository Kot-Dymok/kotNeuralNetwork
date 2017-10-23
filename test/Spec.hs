import Test.Hspec
import KotNeuralNetwork
import KotNeuralNetwork.Internal
import KotNeuralNetwork.ActivationFunctions

main :: IO()
main = hspec $ do
  describe "Test 1" $ do
    it "Test 1 Part 1" $ do
      (10 + 1) `shouldBe` (11 :: Int)
  describe "Calculating errors tests:" $ do
    it "Calculating E_total" $ do
      totalError [2, 3] [0, 1] `shouldBe` (4 :: Double)
    it "Calculating E_partial" $ do
      partialErrors [2, 3] [7, 10] `shouldBe` ([5, 7] :: [Double])
  describe "Perceptron tests:" $ do
    it "Perceptron proceed test" $ do
      let p = Perceptron [4,5,6] activationLogistic activationLogistic' 0.3
      proceed p [-5, 8, -3] `shouldBe` ([0.8807970779778823] :: [Double])
    it "Perceptron learn test" $ do
      let r = 0.3
      let i = [-5, 8, -3]
      let p = Perceptron [4,5,6] activationLogistic activationLogistic' r
      let correction = [0.14500641459649337294]
      let dOut_dNet = 0.10499358540350651735
      let dE_dNet = (*) dOut_dNet $ sum correction
      let w1 = 4 - r * dE_dNet * (-5)
      let w2 = 5 - r * dE_dNet * ( 8)
      let w3 = 6 - r * dE_dNet * (-3)
      let outP = Perceptron [w1,w2,w3] activationLogistic activationLogistic' r
      let outCorrection = map (dE_dNet *) i
      learn p i correction `shouldBe` ((outP, outCorrection) :: (Perceptron, [Double]))
            

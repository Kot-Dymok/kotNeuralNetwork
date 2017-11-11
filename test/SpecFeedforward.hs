module SpecFeedforward where

import Test.Hspec
import KotNeuralNetwork
import KotNeuralNetwork.Internal
import qualified KotNeuralNetwork.ActivationFunctions as AF
import qualified KotNeuralNetwork.Feedforward as FF

-- testFeedforward :: IO()
test = do --hspec $ do
  let aa = AF.ActivationLogistic
  let a  = AF.getFunction aa
  describe "Feedforward network:" $ do
    describe "Perceptron tests:" $ do
      let i = [-5, 8, -3]
      let w = [4,5,6]
      let p = FF.Perceptron w aa
      let r = 0.3
      it "Perceptron proceed test" $ do
        proceed p i `shouldBe` ([0.8807970779778823] :: [Double])
      it "Perceptron learn test" $ do
        let correction = [0.8807970779778823 - 0.5] -- (1/2)*2*(output - correct)*1 == ETot^dOut
        let net = 4*(-5)+5*8+6*(-3)
        let dOut_dNet = (a net) * (1 - a net)
        let dE_dNet = (*) dOut_dNet $ sum correction
        let w1 = 4 - r * dE_dNet * (-5)
        let w2 = 5 - r * dE_dNet * ( 8)
        let w3 = 6 - r * dE_dNet * (-3)
        let outP = FF.Perceptron [w1,w2,w3] aa
        let outCorrection = map (dE_dNet *) w
        learn p i correction 0.3 `shouldBe` ((outP, outCorrection) :: (FF.Perceptron, [Double]))
    describe "Layer tests:" $ do
      let p1 = FF.Perceptron [ 0.5, 0.9,-0.6] aa
      let p2 = FF.Perceptron [ 0.6, 0.7, 0.2] aa
      let p3 = FF.Perceptron [-0.3, 0.1     ] aa
      let p4 = FF.Perceptron [-0.6,-0.4     ] aa
      let l1 = FF.NeuralLayer [p1,p2]
      let l2 = FF.NeuralLayer [p3,p4]
      let i1 = [1,2,3]
      let i2 = [0.6224593312018546, 0.9308615796566533]
      it "Layer proceed test 1" $ do
        proceed l1 i1 `shouldBe` ([0.6224593312018546, 0.9308615796566533] :: [Double])
      it "Layer proceed test 2" $ do
        proceed l2 i2 `shouldBe` ([0.47660418682050537,0.32173273292342686] :: [Double])
      it "Layer learn test 2" $ do
        let p4' = FF.Perceptron [-0.60088561035553930721, -0.40132439279675650824] aa
        let p3' = FF.Perceptron [-0.29891017070256970902, 0.10162979357929022] aa
        let correction = [-1.094676197290966e-3,-2.480628311702152e-3]
        learn l2 i2 [0.47660418682050537-0.5,0.32173273292342686-0.3] 0.3 `shouldBe` (((FF.NeuralLayer [p3',p4']), correction) :: (FF.NeuralLayer,[Double]))
      it "Layer learn test 1" $ do
        let p1' = FF.Perceptron [0.50007717589100663061,0.90015435178201326123,-0.59976847232698] aa
        let p2' = FF.Perceptron [0.60004789470570852631,0.70009578941141705262,0.2001436841171256] aa
        let correction = [-2.244158964281035e-4,-3.432819863397864e-4,1.2242197820757706e-4]
        learn l1 i1 [-1.094676197290966e-3,-2.480628311702152e-3] 0.3 `shouldBe` (((FF.NeuralLayer [p1',p2']), correction) :: (FF.NeuralLayer,[Double]))

    describe "Network tests:" $ do
      let p1 = FF.Perceptron [ 0.5, 0.9,-0.6] aa
      let p2 = FF.Perceptron [ 0.6, 0.7, 0.2] aa
      let p3 = FF.Perceptron [-0.3, 0.1     ] aa
      let p4 = FF.Perceptron [-0.6,-0.4     ] aa
      let l1 = FF.NeuralLayer [p1,p2]
      let l2 = FF.NeuralLayer [p3,p4]
      let nn = FF.NeuralNetwork [l1,l2]
      it "Layer proceed test" $ do
        proceed nn [1,2,3] `shouldBe` ([0.47660418682050537,0.32173273292342686] :: [Double])
      it "Layer learn test" $ do
        let p1' = FF.Perceptron [0.50007717589100663061,0.90015435178201326123,-0.59976847232698] aa
        let p2' = FF.Perceptron [0.60004789470570852631,0.70009578941141705262,0.2001436841171256] aa
        let p3' = FF.Perceptron [-0.29891017070256970902, 0.10162979357929022] aa
        let p4' = FF.Perceptron [-0.60088561035553930721, -0.40132439279675650824] aa
        let l1' = FF.NeuralLayer [p1',p2']
        let l2' = FF.NeuralLayer [p3',p4']
        let nn' = FF.NeuralNetwork [l1',l2']
        let correction = [-2.244158964281035e-4,-3.432819863397864e-4,1.2242197820757706e-4]
        learn nn [1,2,3] [0.5,0.3] 0.3 `shouldBe` ((nn',correction) ::  (FF.NeuralNetwork,[Double]))
            

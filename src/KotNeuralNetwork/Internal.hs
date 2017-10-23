module KotNeuralNetwork.Internal where

import Data.List

class Neural n where
  proceed :: n -> [Double] -> [Double]
  learn   :: n -> [Double] -> [Double]-> (n, [Double])
-- activation :: Double -> Double
-- activation' :: Double -> Double
-- activation = activationLogistic
-- activation' = activationLogistic'
-- learningRate :: Double
-- learningRate = 0.5

partialErrors :: [Double] -> [Double] -> [Double]
partialErrors ideal actual = zipWith (-) actual ideal
totalError :: [Double] -> [Double] -> Double
totalError ideal actual = (*) 0.5 . sum . map (**2) $ partialErrors ideal actual

type Weigth = Double
data Perceptron = Perceptron {
  weigths  :: [Weigth],
  activation :: (Double -> Double),
  activation' :: (Double -> Double),
  learningRate :: Double
  }
  -- deriving Eq

instance Eq Perceptron where
  (==) (Perceptron w a a' r) (Perceptron w1 a1 a1' r1) = (w == w1) && (r == r1)
                                                         -- && (a == a1) && (a' == a1')
instance Show Perceptron where
--  show :: Perceptron -> String
  show (Perceptron w _ _ r) = "Perceptron" ++ " with " ++ wText ++ " and " ++ rText 
    where wText = (++) "weigths: " $ intercalate " " $ map show w
          rText = (++) "learning rate " $ show r

instance Neural Perceptron where
  proceed (Perceptron w a _ _) inputs = [a . sum $ zipWith (*) inputs w]
  learn (Perceptron w a a' r) inputs corrections = (Perceptron w' a a' r, correctionToPreviousLayer)
    where
      dE_dout = sum corrections
      dout_dnet = a' . sum $ zipWith (*) inputs w
      dnet_dw = inputs
      dE_dnet = dE_dout * dout_dnet
      correctionToPreviousLayer = flip map w $ (+) dE_dnet
      wCorrections = flip map dnet_dw $ (+) dE_dnet
      w' = zipWith (-) w $ map (r *) wCorrections

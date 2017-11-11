module KotNeuralNetwork.Internal where

class Neural n where
  -- proceed element input -> output
  proceed :: n -> [Double] -> [Double]
  -- learn element input correction learningRate -> (newElement, newCorrection)
  learn   :: n -> [Double] -> [Double] -> Double -> (n, [Double])

partialErrors :: [Double] -> [Double] -> [Double]
partialErrors ideal actual = zipWith (-) actual ideal
totalError :: [Double] -> [Double] -> Double
totalError ideal actual = (*) 0.5 . sum . map (**2) $ partialErrors ideal actual

type Weigth = Double

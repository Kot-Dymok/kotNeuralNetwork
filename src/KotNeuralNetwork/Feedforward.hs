module KotNeuralNetwork.Feedforward where

import KotNeuralNetwork.Internal
import qualified KotNeuralNetwork.ActivationFunctions as AF

data Perceptron = Perceptron {
  weigths  :: [Weigth],
  activation :: AF.ActivationFunction
  } deriving (Eq, Show)

instance Neural Perceptron where
  proceed (Perceptron w af) inputs = let a = AF.getFunction af in [a . sum $ zipWith (*) inputs w]
  learn (Perceptron w af) inputs corrections r = (Perceptron w' af, correctionToPreviousLayer)
    where
      a' = AF.getFunction' af
      dE_dout = sum corrections
      net = sum $ zipWith (*) inputs w
      dout_dnet = a' net
      dnet_dw = inputs
      dE_dnet = dE_dout * dout_dnet
      correctionToPreviousLayer = flip map w $ (*) dE_dnet
      wCorrections = flip map dnet_dw $ (*) dE_dnet
      w' = zipWith (-) w $ map (r *) wCorrections

data NeuralLayer = NeuralLayer [Perceptron]
  deriving (Show, Eq)

instance Neural NeuralLayer where
  proceed (NeuralLayer p) inputs = concat $ flip map p $ flip proceed inputs
  -- At learn function correction vector is list of summary correction for perceptron.
  -- So, at correction for previous layer we need to sum corrections for perceptron.
  learn (NeuralLayer perceptron) inputs corrections r = (NeuralLayer p', d')
    where
      learnPerceptron :: (Perceptron, Double) -> (Perceptron, [Double])
      learnPerceptron (p, c) = learn p inputs [c] r
      (p', d) = unzip $ map learnPerceptron $ zip perceptron corrections :: ([Perceptron], [[Double]])
      d' = flip foldr1 d $ zipWith (+)

data NeuralNetwork = NeuralNetwork [NeuralLayer]
  deriving (Show, Eq)

instance Neural NeuralNetwork where
  proceed (NeuralNetwork []) _ = []
  proceed (NeuralNetwork (n:[])) inputs = proceed n inputs
  proceed (NeuralNetwork (n:ns)) inputs = proceed (NeuralNetwork ns) $ proceed n inputs
  -- At learn function correction vector is list of summary correction for perceptron. So, at correction for previous layer we need to sum corrections for perceptron.
  -- But we should have not correction at learn function but correct answerd
  learn n@(NeuralNetwork []) _ c _ = (n, c)
  learn (NeuralNetwork (n:[])) inputs correctAnswerd r = let c = partialErrors correctAnswerd $ proceed n inputs
                                                             o = learn n inputs c r
                                                         in (NeuralNetwork [fst o], snd o)
  learn (NeuralNetwork (n:ns)) inputs correctAnswerd r = (NeuralNetwork (n'':n'), c'')
    where
      i' = proceed n inputs -- input for next layer
      (NeuralNetwork n', c') = learn (NeuralNetwork ns) i' correctAnswerd r -- changed next layers and correction for current layer
      (n'', c'') = learn n inputs c' r -- changed current layer and correction to previous layer

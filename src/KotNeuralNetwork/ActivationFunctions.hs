module KotNeuralNetwork.ActivationFunctions where
-- From https://en.wikipedia.org/wiki/Activation_function

activationIdentity :: Double -> Double
activationIdentity x = x
activationIdentity' :: Double -> Double
activationIdentity' _ = 1
activationBinary :: Double -> Double
activationBinary x | x < 0 = 0
                   | otherwise = 1
activationBinary' :: Double -> Double
activationBinary' x | x /= 0 = 0
                    | otherwise = 1
activationLogistic :: Double -> Double
activationLogistic x = 1 / (1 + ( exp $ -x) )
activationLogistic' :: Double -> Double -- Derivative of activation function
activationLogistic' x = (activationLogistic x) * (1 - activationLogistic x)
activationTanH :: Double -> Double
activationTanH = tanh
activationTanH' :: Double -> Double
activationTanH' x =  1 - (activationTanH x) ** 2
activationArcTan :: Double -> Double
activationArcTan x = 1 / (tan x)
activationArcTan' :: Double -> Double
activationArcTan' x = 1 / (x**2 + 1)
activationSoftsign :: Double -> Double
activationSoftsign x = x / (1 + abs x)
activationSoftsign' :: Double -> Double
activationSoftsign' x = (1 + abs x) ** (-2)
activationReLU :: Double -> Double
activationReLU x | x < 0 = 0
                 | otherwise = x
activationReLU' :: Double -> Double
activationReLU' x | x < 0 = 0
                 | otherwise = 1
activationLeakyReLU :: Double -> Double
activationLeakyReLU x | x < 0 = 0.01 * x
                      | otherwise = x
activationLeakyReLU' :: Double -> Double
activationLeakyReLU' x | x < 0 = 0.01
                       | otherwise = 1
activationPReLU :: Double -> Double -> Double
activationPReLU a x | x < 0 = a * x
                    | otherwise = x
activationPReLU' :: Double -> Double -> Double
activationPReLU' a x | x < 0 = a
                    | otherwise = 1
-- activationRReLU' :: Double -> Double -> Double
activationELU :: Double -> Double -> Double
activationELU a x | x < 0 = a * ((exp x) + 1)
                  | otherwise = x
activationELU' :: Double -> Double -> Double
activationELU' a x | x < 0 = a + activationELU a x
                   | otherwise = 1
activationSELU :: Double -> Double
activationSELU x = 1.0507 * activationELU 1.67326 x
activationSELU' :: Double -> Double
activationSELU' x = 1.0507 * activationELU' 1.67326 x
activationSReLU :: Double -> Double -> Double -> Double -> Double -> Double
activationSReLU tl al tr ar x | x <= tl = tl + al * (x - tl)
                              | x >= tr = tr + ar * (x - tr)
                              | otherwise = x
activationSReLU' :: Double -> Double -> Double -> Double -> Double -> Double
activationSReLU' tl al tr ar x | x <= tl = al
                               | x >= tr = ar
                               | otherwise = 1
-- activationAPL :: Double -> Double
activationSoftPlus :: Double -> Double
activationSoftPlus x = log(1 + exp x)
activationSoftPlus' :: Double -> Double
activationSoftPlus' x = 1 / (1 + exp (-x))
activationBentIdentity :: Double -> Double
activationBentIdentity x = ((sqrt $ x**2 + 1) - 1) / 2 + x
activationBentIdentity' :: Double -> Double
activationBentIdentity' x = x / (2 * (sqrt $ x**2 + 1)) + 1
activationSoftExponential :: Double -> Double -> Double
activationSoftExponential a x | a < 0 = - (log (1 - a * (x + a))) / a
                              | a > 0 = ((exp $ a * x) - 1) / a + a
                              | otherwise = x
activationSoftExponential' :: Double -> Double -> Double
activationSoftExponential' a x | a < 0 = 1  / (1 - a * (a + x))
                               | otherwise = exp $ a * x
activationSinusoid :: Double -> Double
activationSinusoid x = sin x
activationSinusoid' :: Double -> Double
activationSinusoid' x = cos x
activationSinc :: Double -> Double
activationSinc x | x == 0 = 1
                 | otherwise = (sin x) / x
activationSinc' :: Double -> Double
activationSinc' x | x == 0 = 0
                  | otherwise = (cos x) / x - (sin x) / (x**2)
activationGaussian :: Double -> Double
activationGaussian x = exp $ (-1) * x**2
activationGaussian' :: Double -> Double
activationGaussian' x = (-2) * x * activationGaussian x

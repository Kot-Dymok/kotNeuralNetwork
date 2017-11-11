module KotNeuralNetwork.ActivationFunctions where
-- From https://en.wikipedia.org/wiki/Activation_function

-- functions is pair function and its derivative

activationIdentity :: (Double -> Double, Double -> Double)
activationIdentity = (\x -> x, \_ -> 1)

activationBinary :: (Double -> Double, Double -> Double)
activationBinary = (\x -> if x < 0 then 0 else 1, \x -> if x /= 0 then 0 else 1)

activationLogistic :: (Double -> Double, Double -> Double)
activationLogistic = (f, f')
  where
    f x = 1 / (1 + ( exp $ -x) )
    f' x = (f x) * (1 - f x)

activationTanH :: (Double -> Double, Double -> Double)
activationTanH = (tanh, \x -> 1 - (tanh x) ** 2)

activationArcTan :: (Double -> Double, Double -> Double)
activationArcTan = (\x -> 1 / (tan x), \x -> 1 / (x**2 + 1))

activationSoftsign :: (Double -> Double, Double -> Double)
activationSoftsign = (\x -> x / (1 + abs x), \x -> (1 + abs x) ** (-2))

activationReLU :: (Double -> Double, Double -> Double)
activationReLU = (\x -> if x < 0 then 0 else x, \x -> if x < 0 then 0 else 1)

activationLeakyReLU :: (Double -> Double, Double -> Double)
activationLeakyReLU = (\x -> if x < 0 then 0.01 * x else x, \x ->if x < 0 then 0.01 else 1)

activationPReLU :: Double -> (Double -> Double, Double -> Double)
activationPReLU a  = (\x -> if x < 0 then a * x else x, \x -> if x < 0 then a else 1)

activationRReLU :: Double -> (Double -> Double, Double -> Double)
activationRReLU a  = (\x -> if x < 0 then a * x else x, \x -> if x < 0 then a else 1)

activationELU :: Double -> (Double -> Double, Double -> Double)
activationELU a = (f, f')
  where
    f = \x -> if x < 0 then a * ((exp x) + 1) else x
    f' = \x -> if x < 0 then a + f x else 1

activationSELU :: (Double -> Double, Double -> Double)
activationSELU = let (elu,elu') = activationELU 1.67326 in (\x -> 1.0507 * elu x, \x -> 1.0507 * elu' x)

activationSReLU :: Double -> Double -> Double -> Double -> (Double -> Double, Double -> Double)
activationSReLU tl al tr ar = (f,f')
  where
    f x | x <= tl = tl + al * (x - tl)
        | x >= tr = tr + ar * (x - tr)
        | otherwise = x
    f' x | x <= tl = al
         | x >= tr = ar
         | otherwise = 1

activationAPL :: [Double] -> [Double] -> (Double -> Double, Double -> Double)
activationAPL as bs = (f,f')
  where
    heaviside = \x -> if maximum [0, x] == x then 1 else 0
    f x = m + sum s
      where
        m = (maximum [0, x])
        s = zipWith (*) as $ map (\b -> maximum [0, b-x]) bs
    f' x = m + sum s
      where
        m = heaviside x
        s = zipWith (*) as $ map (\b -> heaviside b-x) bs
    

activationSoftPlus :: (Double -> Double, Double -> Double)
activationSoftPlus = (\x -> log(1 + exp x), \x -> 1 / (1 + exp (-x)))

activationBentIdentity :: (Double -> Double, Double -> Double)
activationBentIdentity = (\x -> ((sqrt $ x**2 + 1) - 1) / 2 + x,
                          \x -> x / (2 * (sqrt $ x**2 + 1)) + 1)

activationSoftExponential :: Double -> (Double -> Double, Double -> Double)
activationSoftExponential a = (f, f')
  where
    f x | a < 0 = - (log (1 - a * (x + a))) / a
        | a > 0 = ((exp $ a * x) - 1) / a + a
        | otherwise = x
    f' x | a < 0 = 1  / (1 - a * (a + x))
         | otherwise = exp $ a * x

activationSinusoid :: (Double -> Double, Double -> Double)
activationSinusoid = (\x -> sin x, \x -> cos x)

activationSinc :: (Double -> Double, Double -> Double)
activationSinc = (\x -> if x == 0 then 1 else (sin x) / x,
                  \x -> if x == 0 then 0 else (cos x) / x - (sin x) / (x**2))

activationGaussian :: (Double -> Double, Double -> Double)
activationGaussian = (f, f')
  where
    f = \x -> exp $ (-1) * x**2
    f'= \x -> (-2) * x * f x

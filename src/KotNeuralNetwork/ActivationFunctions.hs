module KotNeuralNetwork.ActivationFunctions where
-- From https://en.wikipedia.org/wiki/Activation_function
import Data.Binary

data ActivationFunction = ActivationIdentity
                        | ActivationBinary
                        | ActivationLogistic
                        | ActivationTanH
                        | ActivationArcTan
                        | ActivationSoftsign
                        | ActivationReLU
                        | ActivationLeakyReLU
                        | ActivationPReLU Double
                        | ActivationRReLU Double
                        | ActivationELU Double
                        | ActivationSELU
                        | ActivationSReLU Double Double Double Double
                        | ActivationAPL [Double] [Double]
                        | ActivationSoftPlus
                        | ActivationBentIdentity
                        | ActivationSoftExponential Double
                        | ActivationSinusoid
                        | ActivationSinc
                        | ActivationGaussian
-- | ActivationUnknown (Double->Double, Double->Double) --Temporary disabled: I don't know yet how to do class instance only for that subdata
  deriving (Eq, Show)
   
getFunction :: ActivationFunction -> (Double -> Double)
-- getFunction (ActivationUnknown (f,_)) = f
getFunction (ActivationIdentity) = \x -> x
getFunction (ActivationBinary) = \x -> if x < 0 then 0 else 1
getFunction (ActivationLogistic) = \x -> 1 / (1 + ( exp $ -x) )
getFunction (ActivationTanH) = tanh
getFunction (ActivationArcTan) = \x -> 1 / (tan x)
getFunction (ActivationSoftsign) = \x -> x / (1 + abs x)
getFunction (ActivationReLU) = \x -> if x < 0 then 0 else x
getFunction (ActivationLeakyReLU) = \x -> if x < 0 then 0.01 * x else x
getFunction (ActivationPReLU a) = \x -> if x < 0 then a * x else x
getFunction (ActivationRReLU a) = \x -> if x < 0 then a * x else x
getFunction (ActivationELU a) = \x -> if x < 0 then a * ((exp x) + 1) else x
getFunction (ActivationSELU) = let elu = getFunction (ActivationELU 1.67326) in \x -> 1.0507 * elu x
getFunction (ActivationSReLU tl al tr ar) = \x -> if x <= tl then tl + al * (x - tl) else if x >= tr then tr + ar * (x - tr) else x
getFunction (ActivationAPL as bs) = \x -> (+) (maximum [0, x]) $ sum . zipWith (*) as $ map (\b -> maximum [0, b-x]) bs
getFunction (ActivationSoftPlus) = \x -> log(1 + exp x)
getFunction (ActivationBentIdentity) = \x -> ((sqrt $ x**2 + 1) - 1) / 2 + x
getFunction (ActivationSoftExponential a) | a < 0 = \x -> - (log (1 - a * (x + a))) / a
                                          | a > 0 = \x -> ((exp $ a * x) - 1) / a + a
                                          | otherwise = \x -> x
getFunction (ActivationSinusoid) = sin
getFunction (ActivationSinc) = \x -> if x == 0 then 1 else (sin x) / x
getFunction (ActivationGaussian) = \x -> exp $ (-1) * x**2

getFunction' :: ActivationFunction -> (Double -> Double)
-- getFunction'    (ActivationUnknown (_,f')) = f'
getFunction'    (ActivationIdentity) = \_ -> 1
getFunction'    (ActivationBinary) = \x -> if x /= 0 then 0 else 1
getFunction' af@(ActivationLogistic) =  let f = getFunction af in \x -> (f x) * (1 - f x)
getFunction'    (ActivationTanH) = \x -> 1 - (tanh x) ** 2
getFunction'    (ActivationArcTan) = \x -> 1 / (x**2 + 1)
getFunction'    (ActivationSoftsign) =  \x -> (1 + abs x) ** (-2)
getFunction'    (ActivationReLU) = \x -> if x < 0 then 0 else 1
getFunction'    (ActivationLeakyReLU) = \x ->if x < 0 then 0.01 else 1
getFunction'    (ActivationPReLU a) =  \x -> if x < 0 then a else 1
getFunction'    (ActivationRReLU a) =  \x -> if x < 0 then a else 1
getFunction' af@(ActivationELU a) =  let f = getFunction af in \x -> if x < 0 then a + f x else 1
getFunction'    (ActivationSELU) = let elu' = getFunction' (ActivationELU 1.67326) in \x -> 1.0507 * elu' x
getFunction'    (ActivationSReLU tl al tr ar) = \x -> if x <= tl then al else if x >= tr then ar else 1
getFunction'    (ActivationAPL as bs) = \x -> (+) (heaviside x) $ sum . zipWith (*) as $ map (\b -> heaviside b-x) bs
  where heaviside = \x -> if maximum [0, x] == x then 1 else 0
getFunction'    (ActivationSoftPlus) = \x -> 1 / (1 + exp (-x))
getFunction'    (ActivationBentIdentity) = \x -> x / (2 * (sqrt $ x**2 + 1)) + 1
getFunction'    (ActivationSoftExponential a) | a < 0 = \x -> 1  / (1 - a * (a + x))
                                              | otherwise = \x -> exp $ a * x    
getFunction'    (ActivationSinusoid) = cos
getFunction'    (ActivationSinc) = \x -> if x == 0 then 0 else (cos x) / x - (sin x) / (x**2)
getFunction' af@(ActivationGaussian) = let f = getFunction af in \x -> (-2) * x * f x


instance Binary ActivationFunction where
  put (ActivationIdentity) = put (1 :: Word8)
  put (ActivationBinary) = put (2 :: Word8)
  put (ActivationLogistic) = put (3 :: Word8)
  put (ActivationTanH) = put (4 :: Word8)
  put (ActivationArcTan) = put (5 :: Word8)
  put (ActivationSoftsign) = put (6 :: Word8)
  put (ActivationReLU) = put (7 :: Word8)
  put (ActivationLeakyReLU) = put (8 :: Word8)
  put (ActivationPReLU a) = put (9 :: Word8) >> put a
  put (ActivationRReLU a) = put (10 :: Word8) >> put a
  put (ActivationELU a) = do put (11 :: Word8) >> put a
  put (ActivationSELU) = put (12 :: Word8)
  put (ActivationSReLU tl al tr ar) = do put (13 :: Word8)
                                         put tl
                                         put al
                                         put tr
                                         put ar
  put (ActivationAPL as bs) = put (14 :: Word8) >> put as >> put bs
  put (ActivationSoftPlus) = put (15 :: Word8)
  put (ActivationBentIdentity) = put (16 :: Word8)
  put (ActivationSoftExponential a) = put (17 :: Word8) >> put a
  put (ActivationSinusoid) = put (18 :: Word8)
  put (ActivationSinc) = put (19 :: Word8)
  put (ActivationGaussian) = put (20 :: Word8)
  -- put (ActivationUnknown _) = put (0 :: Word8) 

  get = do t <- get :: Get Word8
           case t of
             -- 0 -> do return (ActivationUnknown (\x->unknown, \x->unknown))
             1  -> do return (ActivationIdentity)
             2  -> do return (ActivationBinary)
             3  -> do return (ActivationLogistic)
             4  -> do return (ActivationTanH)
             5  -> do return (ActivationArcTan)
             6  -> do return (ActivationSoftsign)
             7  -> do return (ActivationReLU)
             8  -> do return (ActivationLeakyReLU)
             9  -> get >>= (\a -> return (ActivationPReLU a))
             10 -> get >>= (\a -> return (ActivationRReLU a))
             11 -> get >>= (\a -> return (ActivationELU a))
             12 -> do return (ActivationSELU)
             13 -> do tl <- get
                      al <- get
                      tr <- get
                      ar <- get
                      return (ActivationSReLU tl al tr ar)
             14 -> do as <- get
                      bs <- get
                      return (ActivationAPL as bs)
             15 -> do return (ActivationSoftPlus)
             16 -> do return (ActivationBentIdentity)
             17 -> get >>= (\a -> return (ActivationSoftExponential a))
             18 -> do return (ActivationSinusoid)
             19 -> do return (ActivationSinc)
             20 -> do return (ActivationGaussian)
             _  -> undefined

import Control.Monad.Random

type Weights = [Double]

eps = 10 ** (-8)

updateList :: [a] -> Int -> a -> [a]
updateList list index newElem =
    zipWith match list [0..]
  where
    match elem currentIndex =
      if currentIndex == index
      then newElem
      else elem

norm :: Weights -> Double
norm xs = sqrt $ sum $ map (**2) xs

checkGrad :: MonadRandom m => (Weights -> Double) -> (Weights -> Weights) -> m (Weights, Weights)
checkGrad f f' =
  do
    -- TODO: ensure neither f nor f' depend on weights length.
    randomWeights <- take 10 <$> getRandoms
    let perturb elem index op = f $ updateList randomWeights index (elem `op` eps)
    let numericalDerivative = zipWith (\elem index -> (perturb elem index (+) - perturb elem index (-)) / (2 * eps)) randomWeights [0..]
    let estimatedDerivative = f' randomWeights
    let diffNorm = norm $ zipWith (-) numericalDerivative estimatedDerivative
    let sumNorm  = norm $ zipWith (+) numericalDerivative estimatedDerivative
    -- a list of random weights
    -- compute derivative for each of them
    -- compare with result of f' for that particular weight
    -- return $ diffNorm / sumNorm
    return (numericalDerivative, estimatedDerivative)


f1 :: [Double] -> Double
f1 = sum

f1' :: [Double] -> [Double]
f1' = map (\_ -> 1)

f2 :: [Double] -> Double
f2 = sum . map (**2)

f2' :: [Double] -> [Double]
f2' = map (2*)

import Control.Monad.Random (getRandoms, MonadRandom)

import Numeric.LinearAlgebra ((|>), sumElements, norm_2)
import Numeric.LinearAlgebra.Data (fromList, toList, size, R, Vector)

eps = 10 ** (-8)

updateList :: Vector R -> Int -> R -> Vector R
updateList vector index newElem =
    fromList $ zipWith match (toList vector) [0..]
  where
    match elem currentIndex =
      if currentIndex == index
      then newElem
      else elem

checkGrad :: MonadRandom m => (Vector R -> R) -> (Vector R -> Vector R) -> m (Vector R, Vector R)
checkGrad f f' =
   do
     -- TODO: ensure neither f nor f' depend on weights length.
     randomWeights <- fromList <$> take 10 <$> getRandoms
     let perturb elem index op = f $ updateList randomWeights index (elem `op` eps)
     let numericalDerivative = fromList $ zipWith (\elem index -> (perturb elem index (+) - perturb elem index (-)) / (2 * eps)) (toList randomWeights) [0..]
     let estimatedDerivative = f' randomWeights
     let diffNorm = norm_2 $ numericalDerivative - estimatedDerivative
     let sumNorm  = norm_2 $ numericalDerivative + estimatedDerivative
--     -- a list of random weights
--     -- compute derivative for each of them
--     -- compare with result of f' for that particular weight
--     -- return $ diffNorm / sumNorm
     return (numericalDerivative, estimatedDerivative)

-- Some simple functions and their gradients

f1 :: Vector R -> R
f1 v = sumElements v

f1' :: Vector R -> Vector R
f1' v = size v |> repeat 1

f2 :: Vector R -> R
f2 v = sumElements $ v ** 2

f2' :: Vector R -> Vector R
f2' v = 2 * v

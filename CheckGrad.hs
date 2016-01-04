import Control.Monad.Random (getRandoms, MonadRandom)
import Numeric.LinearAlgebra ((|>), norm_2, sumElements)
import Numeric.LinearAlgebra.Data (
    Matrix,
    R,
    Vector,
    asRow,
    cmap,
    diagl,
    fromList,
    ident,
    scalar,
    size,
    toColumns,
    toList,
    toRows)

eps :: R
eps = 10 ** (-8)
dim = 10

checkGrad :: MonadRandom m => (Vector R -> R) -> (Vector R -> Vector R) -> m (Vector R, Vector R)
checkGrad f f' =
   do
     -- TODO: ensure neither f nor f' depend on weights length.
     randomWeights <- fromList <$> take dim <$> getRandoms
     let epsMatrix = scalar eps * ident dim
     let perturb op = fromList $ map f $ toRows $ asRow randomWeights `op` epsMatrix
     let numericalDerivative = (perturb (+) - perturb (-)) / scalar (2 * eps)
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

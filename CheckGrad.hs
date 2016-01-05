import Control.Monad.Random (getRandoms, MonadRandom)
import Data.Eq.Approximate (AbsolutelyApproximateValue (..), Digits)
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
import TypeLevel.NaturalNumber (Seven)

type ApproximateDouble = AbsolutelyApproximateValue (Digits Seven) Double

wrapAD :: Double -> ApproximateDouble
wrapAD = AbsolutelyApproximateValue

eps :: R
eps = 1e-8
dim = 10

approxEquality :: Double -> Double -> Bool
approxEquality a b = (wrapAD a) == (wrapAD b)

approxEqualVectors :: Vector Double -> Vector Double -> Bool
approxEqualVectors a b =
  and $ zipWith approxEquality (toList a) (toList b)

checkGrad :: MonadRandom m => (Vector R -> R) -> (Vector R -> Vector R) -> m Bool
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
    -- return $ approxEqualVectors numericalDerivative estimatedDerivative
    return $ diffNorm / sumNorm < 1e-7

-- Some simple functions and their gradients

f1 :: Vector R -> R
f1 v = sumElements v

f1' :: Vector R -> Vector R
f1' v = size v |> repeat 1

f2 :: Vector R -> R
f2 v = sumElements $ v ** 2

f2' :: Vector R -> Vector R
f2' v = 2 * v


type Output = Float
type Weight = Float
type Input = Float
type Activation = Float -> Float
type Layer = [Input] -> [Output]

neuron :: Activation -> [Weight] -> [Input] -> Output
neuron f ws xs = f $ sum $ zipWith (*) ws xs

layer :: Activation -> [[Weight]] -> Layer
layer f wss xs = map (\ws -> neuron f ws xs) wss

-- forward :: Activation -> [[[Weight]]] -> [Input] -> [Output]
forward :: [Layer] -> [Input] -> [Output]
forward fs xs = foldl (\os f -> f os) xs fs

-- train :: (Activation, Activation) ->

w = [1,1,1,1,1]
layer1Weights = [w,w,w,w,w]
layer2Weights = [w,w,w,w,w,w,w]

sigma :: Float -> Float
sigma x = 1 / (1 + exp (-x))

network =
  [
    layer sigma layer1Weights,
    layer sigma layer2Weights,
    layer sigma layer1Weights,
    layer sigma layer1Weights,
    layer sigma layer1Weights
  ]

initialData :: [Input]
initialData = [1,2,3,4,5]

result = forward network initialData

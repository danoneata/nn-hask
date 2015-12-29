type Output = Float
type Weight = Float
type Input = Float
type Activation = Float -> Float
type Neuron = [Weight] -> [Input] -> Output
type Layer = [[Weight]] -> [Input] -> [Output]
type NeuralNet = [(Layer, [[Weight]])]

dotProduct xs ys = sum $ zipWith (*) xs ys

outerProduct :: [Float] -> [Float] -> [[Float]]
outerProduct xs ys = [[x * y | y <- ys] | x <- xs]

neuron :: Activation -> Neuron
neuron f ws xs = f $ dotProduct ws xs

layer :: Activation -> Layer
layer f wss xs = map (\ws -> neuron f ws xs) wss

forward :: NeuralNet -> [Input] -> [Output]
forward fs xs = foldl (\os (f, weights) -> f weights os) xs fs

train :: NeuralNet -> [Input] -> [Bool] -> (Float -> Float) -> NeuralNet
train network input expectedOutput derivative =
  let
    intermediateOutputs :: [([Output], [[Weight]])]
    intermediateOutputs =
      tail $ scanl (\(input, _) (layer, layerWeights) -> (layer layerWeights input, layerWeights)) (input, []) network
    finalOutput = fst . last $ intermediateOutputs
    deltaWeights = backprop finalOutput expectedOutput intermediateOutputs derivative
  in
    updateWeights network deltaWeights

updateWeights :: NeuralNet -> [[[Weight]]] -> NeuralNet
updateWeights net weights =
  zipWith (\(layer, oldWeights) deltaWeights -> (layer, zipWith (zipWith (-)) oldWeights deltaWeights)) net weights

backprop :: [Output] -> [Bool] -> [([Output], [[Weight]])] -> (Float -> Float) -> [[[Weight]]]
backprop actualOutput expectedOutput intermediateOutputs derivative =
  let
    toInt bool = if bool then 1 else 0
    deltaOutput = zipWith (-) actualOutput (map toInt expectedOutput)
    seed = (deltaOutput :: [Float], [] :: [[[Weight]]])
    fold (acc @ (delta, weights)) (curr @ (intermediateOutput, prevWeights)) =
      let
        newDelta = zipWith (*) (map derivative intermediateOutput) (map (\prevWeight -> dotProduct prevWeight delta) prevWeights)
        deltaWeight = outerProduct delta intermediateOutput
      in
        (newDelta, deltaWeight : weights)
  in
    reverse $ snd $ foldl fold seed intermediateOutputs

w = [1,1,1,1,1]
layer1Weights = [w,w,w,w,w]
layer2Weights = [w,w,w,w,w,w,w]

-- TODO: this signature vs just Activation.
sigma :: Floating a => a -> a
sigma x = 1 / (1 + exp (-x))

network :: NeuralNet
network =
  [
    (layer sigma, layer1Weights),
    (layer sigma, layer2Weights),
    (layer sigma, layer1Weights),
    (layer sigma, layer1Weights),
    (layer sigma, layer1Weights)
  ]

initialData :: [Input]
initialData = [1,2,3,4,5]

result = forward network initialData

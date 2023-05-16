module Main where

import Control.Monad.State
import qualified Data.Map as Map

data (Fractional a, Eq a) => Tensor0D a = Tensor0D
  { tid :: Int,
    value :: a
  }
  deriving (Show, Eq)

data Operator = MP | DV | AD | NA
  deriving (Eq)

data (Fractional a, Eq a) => Operation a = Operation Operator (Tensor0D a) (Tensor0D a) (Tensor0D a)
  deriving (Eq)

data (Fractional a, Eq a) => Tape a = Tape
  {
    operations:: [Operation a],
    nextTensorId:: Int
  }
newTape = Tape [] 0

data (Fractional a, Eq a) => TensorTree a = Empty | Cons (Tensor0D a) Operator (TensorTree a) (TensorTree a) deriving (Eq)

createTensor :: (Fractional a, Eq a) => a -> State (Tape a) (Tensor0D a)
createTensor value = do
  tape <- get
  let tensorId = nextTensorId tape
  put $ tape {nextTensorId = tensorId + 1}
  return $ Tensor0D tensorId value 

applyGrads :: (Fractional a) => Operator -> a -> a -> a -> (a, a)
applyGrads op parentGrads leftValue rightValue
  | op == MP = (parentGrads * rightValue, parentGrads * leftValue)
  | op == DV = (parentGrads * (1 / rightValue), parentGrads * (-1) * (leftValue / (rightValue * rightValue)))
  | op == AD = (parentGrads, parentGrads)

tAdd :: (Fractional a, Eq a) => Tensor0D a -> Tensor0D a -> State (Tape a) (Tensor0D a)
tAdd t1@Tensor0D { tid = id1, value = value1 } t2@Tensor0D { tid = id2, value = value2 } = do
  tape <- get
  let tensorId = nextTensorId tape
  let ops = operations tape
  let newTensor = Tensor0D tensorId (value1 + value2)
  put $ tape {nextTensorId = tensorId + 1, operations = Operation AD newTensor t1 t2 : ops}
  return newTensor

tMul :: (Fractional a, Eq a) => Tensor0D a -> Tensor0D a -> State (Tape a) (Tensor0D a)
tMul t1@Tensor0D { tid = id1, value = value1 } t2@Tensor0D { tid = id2, value = value2 } = do
  tape <- get
  let tensorId = nextTensorId tape
  let ops = operations tape
  let newTensor = Tensor0D tensorId (value1 * value2)
  put $ tape {nextTensorId = tensorId + 1, operations = Operation MP newTensor t1 t2 : ops}
  return newTensor

tDiv :: (Fractional a, Eq a) => Tensor0D a -> Tensor0D a -> State (Tape a) (Tensor0D a)
tDiv t1@Tensor0D { tid = id1, value = value1 } t2@Tensor0D { tid = id2, value = value2 } = do
  tape <- get
  let tensorId = nextTensorId tape
  let ops = operations tape
  let newTensor = Tensor0D tensorId (value1 / value2)
  put $ tape {nextTensorId = tensorId + 1, operations = Operation DV newTensor t1 t2 : ops}
  return newTensor

appendTree :: (Fractional a, Eq a) => Operation a -> TensorTree a -> TensorTree a
appendTree (Operation op t1 t2 t3) Empty = Cons t1 op (Cons t2 NA Empty Empty) (Cons t3 NA Empty Empty)
appendTree fullOp@(Operation op t1@Tensor0D { tid = opId } t2 t3) tree@(Cons treeTop@Tensor0D { tid = id } treeOp leftTree@(Cons Tensor0D { tid = leftId, value = leftValue } _ _ _) rightTree@(Cons Tensor0D { tid = rightId, value = rightValue } _ _ _))
  | opId == leftId = Cons treeTop treeOp (Cons t1 op (Cons t2 NA Empty Empty) (Cons t3 NA Empty Empty)) rightTree
  | opId == rightId = Cons treeTop treeOp leftTree (Cons t1 op (Cons t2 NA Empty Empty) (Cons t3 NA Empty Empty))
  | otherwise =
    let newLeftTree = appendTree fullOp leftTree
        newRightTree = appendTree fullOp rightTree
    in if newLeftTree /= leftTree
          then Cons treeTop treeOp newLeftTree rightTree
          else Cons treeTop treeOp leftTree newRightTree
appendTree _ tree@(Cons _ _ Empty Empty) = tree

buildTree :: (Fractional a, Eq a) => [Operation a] -> TensorTree a -> TensorTree a
buildTree (x:y) tree = buildTree y $ appendTree x tree
buildTree _ tree = tree

backTree :: (Fractional a, Eq a) => TensorTree a -> Map.Map Int a -> Map.Map Int a
backTree (Cons Tensor0D { tid = id } op leftTree@(Cons Tensor0D { tid = leftId, value = leftValue } _ _ _) rightTree@(Cons Tensor0D { tid = rightId, value = rightValue } _ _ _)) map =
  let pGrads = Map.findWithDefault 1 id map
      (leftGrads, rightGrads) = applyGrads op pGrads leftValue rightValue 
      leftMap = Map.delete id $ Map.insert leftId leftGrads map
      rightMap = Map.insert rightId rightGrads map
  in Map.unionWith (+) (backTree leftTree leftMap) (backTree rightTree rightMap)
backTree (Cons Tensor0D { tid = id } op Empty Empty) map = map

backward :: (Fractional a, Eq a) => State (Tape a) (Map.Map Int a)
backward = do
  tape <- get
  let ops = operations tape
  let tree = buildTree ops Empty
  return $ backTree tree Map.empty

doComputations :: (Fractional a, Eq a) => State (Tape a) (Tensor0D a, Map.Map Int a)
doComputations = do
  t0 <- createTensor 1.5
  t1 <- createTensor 2.5
  t2 <- createTensor 3.5
  t3 <- createTensor 4.5
  t4 <- tMul t0 t1
  t5 <- tDiv t4 t2
  t6 <- tAdd t5 t3
  grads <- backward
  return (t6, grads)

main :: IO ()
main = do
  let (tensor, grads) = evalState doComputations newTape 
  print tensor 
  print grads

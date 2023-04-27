module Main where

import Control.Monad.State
import qualified Data.Map as Map
import Debug.Trace (trace)

data (Num a, Eq a) => Tensor0D a = Tensor0D
  { tid :: Int,
    value :: a
  }
  deriving (Show, Eq)

data Operator = MP | DV | AD | NA
  deriving (Show, Eq)

data (Num a, Eq a) => Operation a = Operation Operator (Tensor0D a) (Tensor0D a) (Tensor0D a) 
  deriving (Show, Eq)

data (Num a, Eq a) => Tape a = Tape 
  {
    operations:: [Operation a],
    nextTensorId:: Int
  }
  deriving (Show)

data (Num a, Eq a) => TensorTree a = Empty | Cons (Tensor0D a) Operator (TensorTree a) (TensorTree a) deriving (Eq)

mul :: (Num a, Eq a) => Tensor0D a -> Tensor0D a -> State (Tape a) (Tensor0D a)
mul t1@Tensor0D { tid = id1, value = value1 } t2@Tensor0D { tid = id2, value = value2 } = do
  tape <- get
  let tensorId = nextTensorId tape
  let ops = operations tape
  let newTensor = Tensor0D tensorId (value1 * value2)
  put $ tape {nextTensorId = tensorId + 1, operations = Operation MP newTensor t1 t2 : ops}
  return newTensor 

appendTree :: (Num a, Eq a) => Operation a -> TensorTree a -> TensorTree a
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

buildTree :: (Num a, Eq a) => [Operation a] -> TensorTree a -> TensorTree a
buildTree (x:y) tree = buildTree y $ appendTree x tree 
buildTree _ tree = tree

backTree :: (Num a, Eq a, Show a) => TensorTree a -> Map.Map Int a -> Map.Map Int a
backTree (Cons Tensor0D { tid = id } op leftTree@(Cons Tensor0D { tid = leftId, value = leftValue } _ _ _) rightTree@(Cons Tensor0D { tid = rightId, value = rightValue } _ _ _)) map = 
  let pGrads = trace ("\nbackTree " ++ show id ++ " " ++ show leftId ++ " " ++ show rightId ++ " "++ show (Map.findWithDefault 1 id map)) (Map.findWithDefault 1 id map)
      leftGrads = trace ("leftGrads " ++ show (pGrads * rightValue)) pGrads * rightValue
      rightGrads = trace ("rightGrads " ++ show (pGrads * leftValue)) pGrads * leftValue
      leftMap = Map.delete id $ Map.insert leftId leftGrads map
      rightMap = Map.insert rightId rightGrads map
  in Map.unionWith (+) (backTree leftTree leftMap) (backTree rightTree rightMap) 
backTree (Cons Tensor0D { tid = id } op Empty Empty) map = map

backward :: (Num a, Eq a, Show a) => State (Tape a) (Map.Map Int a)
backward = do
  tape <- get
  let ops = operations tape
  let tree = buildTree ops Empty
  return $ backTree tree Map.empty

doComputations :: (Num a, Eq a, Show a) => State (Tape a) (Tensor0D a, Map.Map Int a)
doComputations = do
  let t0 = Tensor0D 0 1
  let t1 = Tensor0D 1 2
  t2 <- mul t0 t1
  t3 <- mul t1 t2
  t4 <- mul t2 t3
  t5 <- mul t4 t3
  grads <- backward
  return (t5, grads)

main :: IO ()
main = do
  let out = evalState doComputations (Tape [] 2)
  print out

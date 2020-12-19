{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module ProcessTree where

import Control.Lens hiding (children, folding, (:<))
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Lang
import NameGen
import Substitution

import Debug.Trace

type Node = Int

data NodeData = NodeData
  { _expression :: Exp,
    _parent :: Maybe Node,
    _children :: [Node],
    _foldingTo :: Maybe (Node, [(Variable, Variable)]),
    _foldings :: [(Node, [(Variable, Variable)])]
  }
  deriving (Show)

data ProcessTree = ProcessTree
  { _unprocessedLeaves :: IntSet,
    _nodes :: IntMap NodeData,
    _counter :: Int,
    _definitions :: [Definition]
  }
  deriving (Show)

makeLenses ''ProcessTree

makeLenses ''NodeData

initialProcessTree (Program e defs) =
  ProcessTree
    { _unprocessedLeaves = IntSet.singleton 0,
      _nodes = IntMap.singleton 0 initialNode,
      _counter = 1,
      _definitions = defs
    }
  where
    initialNode =
      NodeData
        { _expression = e,
          _parent = Nothing,
          _children = [],
          _foldingTo = Nothing,
          _foldings = []
        }

popUnprocessedLeaf :: MonadState ProcessTree m => m (Maybe Node)
popUnprocessedLeaf = do
  leafs <- use unprocessedLeaves
  unprocessedLeaves %= IntSet.deleteMin
  case IntSet.minView leafs of
    Nothing -> return Nothing
    Just (a, _) -> return $ Just a

getExpression node =
  fromMaybe (error $ "Invalid node " ++ show node) <$> preuse (nodes . ix node . expression)

addOneChildren node expr = do
  cnt <- use counter
  nodes %= IntMap.insert cnt newNode
  counter += 1
  unprocessedLeaves %= IntSet.insert cnt
  nodes . ix node . children %= (++ [cnt])
  where
    newNode =
      NodeData
        { _expression = expr,
          _parent = Just node,
          _children = [],
          _foldingTo = Nothing,
          _foldings = []
        }

addChildren node exprs = mapM_ (addOneChildren node) exprs

getAncestors node = do
  nodeData <- fromMaybe (error $ "Invalid node" ++ show node) <$> preuse (nodes . ix node)
  let p = nodeData ^. parent
  case p of
    Just pNode -> (pNode :) <$> getAncestors pNode
    Nothing -> return []
  

getParent node = fromMaybe (error $ "Invalid node " ++ show node) <$> preuse (nodes . ix node . parent)

getChildren node = fromMaybe (error $ "Invalid node " ++ show node) <$> preuse (nodes . ix node . children)

getFoldings node = fromMaybe (error $ "Invalid node " ++ show node) <$> preuse (nodes . ix node . foldings)

getFoldingTo node = fromMaybe (error $ "Invalid node " ++ show node) <$> preuse (nodes . ix node . foldingTo)

getDefinitions :: MonadState ProcessTree m => m [Definition]
getDefinitions = use definitions

foldTo node to subst = do
  nodes . ix node . foldingTo .= Just (to, subst)
  nodes . ix to . foldings %= ((node, subst) :)

deleteSubtree node = do
  removeFolding
  removeFromUnprocessed
  getChildren node >>= mapM deleteSubtree
  removeFromNodes
  return ()
  where
    removeFolding = do
      folding <- getFoldingTo node
      case folding of
        Nothing -> return ()
        Just (to, subst) -> do
          nodes . ix to . foldings %= List.deleteBy (\(a, _) (b, _) -> a == b) (to, subst)
    removeFromUnprocessed = do
      unprocessedLeaves %= IntSet.delete node
    removeFromNodes = do
      nodes %= IntMap.delete node

replaceNodeWith node expr = do
  parent <- getParent node
  deleteSubtree node
  nodes %= IntMap.insert node (newNode parent)
  unprocessedLeaves %= IntSet.insert node
  where
    newNode parent = NodeData {
      _expression = expr,
      _parent = parent,
      _children = [],
      _foldingTo = Nothing,
      _foldings = []
    }


newtype Env a = Env {runEnv :: (StateT ProcessTree (State NameGen) a)}
  deriving (Functor, Applicative, Monad, MonadState ProcessTree)

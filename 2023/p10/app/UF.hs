module UF where

import Data.Map (Map, empty, lookup, size)
import Data.Map.Strict (insertWith)
import Data.Maybe (Maybe (Nothing), fromJust)
import Prelude hiding (lookup)

data Node k = Node Integer k (Maybe (Node k))

nodeId :: Node k -> Integer
nodeId (Node id _ _) = id

data UnionFind k = UnionFind (Map k (Node k))

mkUnionFind :: (Ord k) => UnionFind k
mkUnionFind = UnionFind empty

findId :: (Ord k) => Node k -> UnionFind k -> (Node k, UnionFind k)
findId
  ( Node
      id
      key
      ( Just
          parent@(Node _ _ (Just grandparent))
        )
    )
  (UnionFind uf) = findId parent (insert key (Node id key grandparent))
findId (Node _ _ (Just (Node parentId _ _))) = parentId
findId (Node id _ Nothing) = id

find :: (Ord k) => k -> UnionFind k -> (Integer, UnionFind k)
find id (UnionFind uf) =
  let newMap = insertWith (\node _ -> node) id (Node (toInteger $ size uf) Nothing) uf
      (node, newMap2) = findId (fromJust $ lookup id newMap) newMap
   in (nodeId node, newMap2)

union :: (Ord k) => k -> k -> UnionFind k -> UnionFind k
union k1 k2 uf =
  let (id1, uf1) = find k1 uf
      (id2, uf2) = find k2 uf1
   in 0

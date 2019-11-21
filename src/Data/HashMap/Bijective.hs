{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}

{- |
 Module : Data.HashMap.Bijective
 Description : A strict bijective HashMap implementation. 
 Copyright : Rose <rose@empty.town>
 License : BSD3
 Maintainer : rose@empty.town 
-}

module Data.HashMap.Bijective (
  BiHashMap,
  empty,
  singleton,
  null,
  size,
  member,
  memberR,
  lookup,
  lookupR,
  lookupDefault,
  lookupDefaultR,
  insert,
  delete,
  deleteR,
  adjust,
--  update,
--  updateR,
  toList,
  fromList
) where

import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Tuple (swap)
import Prelude hiding (null, lookup)

data BiHashMap k v = BiHashMap !(HM.HashMap k v) !(HM.HashMap v k)
  deriving (Generic, NFData)

type Hashables k v = (Hashable k, Hashable v)
type Showables k v = (Show k, Show v)
type Equals    k v = (Eq k, Eq v)

-- | /O(1)/. Create an empty BiHashMap.
empty :: (Hashables k v) => BiHashMap k v
empty = BiHashMap HM.empty HM.empty

-- | /O(1)/. Create a BiHashMap containing a single element.
singleton :: Hashables k v => k -> v -> BiHashMap k v
singleton k v = BiHashMap (HM.singleton k v) (HM.singleton v k)

-- | /O(1)/. Returns true if the map is empty.
null :: Hashables k v => BiHashMap k v -> Bool
null (BiHashMap l _) = HM.null l

-- | /O(1)/. Count of elements in the map.
size :: Hashables k v => BiHashMap k v -> Int
size (BiHashMap l _) = HM.size l

-- | /O(log n)/. Returns true if the map contains the key.
member :: (Eq k, Hashables k v) => k -> BiHashMap k v -> Bool
member k (BiHashMap l _) = HM.member k l

-- | /O(log n)/. 'member' specialized for the value type.
memberR :: (Eq v, Hashables k v) => v -> BiHashMap k v -> Bool
memberR v (BiHashMap _ r) = HM.member v r

-- | /O(log n)/. Look up a value by key.
lookup :: (Eq k, Hashable k) => k -> BiHashMap k v -> Maybe v
lookup k (BiHashMap l _) = HM.lookup k l

-- | /O(log n)/. Look up a key by value.
lookupR :: (Eq v, Hashable v) => v -> BiHashMap k v -> Maybe k
lookupR v (BiHashMap _ r) = HM.lookup v r

-- | /O(log n)/. Look up a value by key with a default if nonexistent.
lookupDefault :: (Eq k, Hashables k v) => v -> k -> BiHashMap k v -> v
lookupDefault d k (BiHashMap l _) = HM.lookupDefault d k l

-- | /O(log n)/. Look up a key by value with a default if nonexistent.
lookupDefaultR :: (Eq v, Hashables k v) => k -> v -> BiHashMap k v -> k
lookupDefaultR d v (BiHashMap _ r) = HM.lookupDefault d v r

-- | /O(log n)/. Insert an element into the map.
insert :: (Equals k v, Hashables k v) => k -> v -> BiHashMap k v -> BiHashMap k v
insert k v (BiHashMap l r) = BiHashMap (HM.insert k v l) (HM.insert v k r)

-- | /O(log n)/. Delete an element from the map by key.
delete :: (Equals k v, Hashables k v) => k -> BiHashMap k v -> BiHashMap k v
delete k m@(BiHashMap l r) =
  case HM.lookup k l of
    Just v  -> BiHashMap (HM.delete k l) (HM.delete v r)
    Nothing -> m

-- | /O(log n)/. Delete an element from the map by value.
deleteR :: (Equals k v, Hashables k v) => v -> BiHashMap k v -> BiHashMap k v
deleteR v m@(BiHashMap l r) =
  case HM.lookup v r of
    Just k  -> BiHashMap (HM.delete k l) (HM.delete v r)
    Nothing -> m

-- | /O(log n)/. Modify a value in the map at a key with an updater function.
-- If the key is not present within the map, the map goes unmodified.
adjust :: (Equals k v, Hashables k v) => (v -> v) -> k -> BiHashMap k v -> BiHashMap k v
adjust f k m@(BiHashMap l r) =
  case HM.lookup k l of
    Just v  -> let nv = f v in
      BiHashMap (HM.insert k nv l) (HM.insert nv k $ HM.delete v r)
    Nothing -> m

-- | /O(log n)/. A version of 'adjust' specialized for the value type.
adjustR :: (Equals k v, Hashables k v) => (k -> k) -> v -> BiHashMap k v -> BiHashMap k v
adjustR f v m@(BiHashMap l r) =
  case HM.lookup v r of
    Just k  -> let nk = f k in
      BiHashMap (HM.insert nk v $ HM.delete k l) (HM.insert v nk r)
    Nothing -> m 

-- | /O(log n)/. Update the value at a key, if present.
-- 'Nothing' results in the element being deleted.
-- update :: (Equals k v, Hashables k v) => (v -> Maybe v) -> k -> BiHashMap k v -> BiHashMap k v
-- update f k (BiHashMap l r) = BiHashMap left right
--   where
--     vs@(v', nv') = (,) <$> HM.lookup k l <*> HM.lookup k l >>= f
--     left = maybe (HM.delete k l) (\nv -> HM.insert k nv l) nv'
--     right = case v' of
--       Just v  -> maybe (HM.delete (fst vs) r) (\nv -> HM.insert nv k $ HM.delete v r) vs
--       Nothing -> r
        
-- updateR :: (k -> Maybe k) -> v -> BiHashMap k v -> BiHashMap k v
-- updateR f v (BiHashMap l r) = undefined
                             
-- | /O(n)/. Return a list of the map's elements.
toList :: Hashables k v => BiHashMap k v -> [(k, v)]
toList (BiHashMap l _) = HM.toList l

-- | /O(n * log n)/. Return a BiHashMap built from a list of two-tuples.
fromList :: (Equals k v, Hashables k v) => [(k, v)] -> BiHashMap k v
fromList pairs = BiHashMap left right
  where left  = HM.fromList pairs
        right = HM.fromList $ map swap pairs

instance (Hashables k v, Showables k v) => Show (BiHashMap k v) where
  show = ("fromList " <>) . show . toList 

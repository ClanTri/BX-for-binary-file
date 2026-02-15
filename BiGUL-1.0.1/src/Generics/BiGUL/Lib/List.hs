-- | A library for processing lists in BiGUL.

module Generics.BiGUL.Lib.List where

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Control.Arrow ((***))
import Data.Maybe (isJust, catMaybes)


-- | List alignment. Operating only on the sources satisfying the source condition,
--   and using the specified matching condition, 'align' finds for each view the first matching source
--   that has not been matched with previous views, and updates the source using the inner program.
--   If there is no matching source, one is created using the creation argument —
--   after creation, the created source should match with the view as determined by the matching condition.
--   For a source not matched with any view, the concealment argument is applied —
--   if concealment computes to @Nothing@, the source is deleted;
--   if concealment computes to @Just s'@, where @s'@ should not satisfy the source condition,
--   the source is replaced by @s'@.
align :: (Show a, Show b)
      => (a -> Bool)       -- ^ source condition
      -> (a -> b -> Bool)  -- ^ matching condition
      -> BiGUL a b         -- ^ inner program
      -> (b -> a)          -- ^ creation
      -> (a -> Maybe a)    -- ^ concealment
      -> BiGUL [a] [b]
align p match b create conceal = Case
  [ $(normalSV [| null . filter p |] [p| [] |] [| null . filter p |])
    ==> $(rearrV [| \[] -> () |])$
          skip ()
  , $(adaptiveSV [p| _ |] [p| [] |])
    ==> \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)
  -- view is necessarily nonempty in the cases below
  , $(normalSV [p| (p -> False):_ |] [p| _ |] [p| (p -> False):(null . filter p -> False) |])
    ==> $(rearrS [| \(s:ss) -> ss |])$
          align p match b create conceal
  , $(normal [| \(s:ss) (v:vs) -> p s && match s v |] [p| (p -> True):_ |])
    ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = align p match b create conceal |])
  , $(adaptive [| \ss (v:_) -> isJust (findFirst (\s -> p s && match s v) ss) ||
                               let s = create v in p s && match s v |])
    ==> \ss (v:_) -> maybe (create v:ss) (uncurry (:)) (findFirst (\s -> p s && match s v) ss)
  ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findFirst p [] = Nothing
    findFirst p (x:xs) | p x       = Just (x, xs)
    findFirst p (x:xs) | otherwise = fmap (id *** (x:)) (findFirst p xs)



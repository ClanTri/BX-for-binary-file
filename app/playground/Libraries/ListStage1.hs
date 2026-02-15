{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ListStage1 where

import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH




-- work on non-empty list, remove the first maximum value
removeMaximum :: (Ord a) => [a] -> [a]
removeMaximum [x] = []
removeMaximum xs = let m = maximum xs in takeWhile (/= m) xs ++ tail (dropWhile (/= m) xs)

secondMax :: (Ord a) => [a] -> a
secondMax [x] = x
secondMax xs = maximum (removeMaximum xs)

removeMinimum :: (Ord a) => [a] -> [a]
removeMinimum [x] = []
removeMinimum xs = let m = minimum xs in takeWhile (/= m) xs ++ tail (dropWhile (/= m) xs)

secondMin :: (Ord a) => [a] -> a
secondMin [x] = x
secondMin xs = minimum (removeMinimum xs)


fromJust (Just x) = x
fromJust Nothing = error "the value \"Nothing\" detected."


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Left detected!"

sameElems :: (Eq a) => a -> [a] -> Bool
sameElems _ [] = True
sameElems x xs = and $ map (== x) xs

sameElems' :: (Eq a) => [a] -> Bool
sameElems' [] = True
sameElems' (x:xs) = sameElems x xs

isTails :: Eq a => [[a]] -> Bool
isTails [[]]         = True
isTails (xs:ys:[])   = if tail xs == ys then True else False
isTails (xs:ys:xyss) = if tail xs == ys then isTails (ys:xyss) else False
isTails _            = False

isInits :: Eq a => [[a]] -> Bool
isInits [[]]         = True
isInits (xs:ys:[])   = if xs == init ys then True else False
isInits (xs:ys:xyss) = if xs == init ys then isInits (ys:xyss) else False
isInits _            = False

inits2list :: (Eq a) => [[a]] -> [a]
inits2list xss = if isInits xss then last xss else error "the view is not a valid inits"


-- yet another isomorphism
lensCons :: BiGUL (a, [a]) [a]
lensCons =
  Case  [ $(adaptive  [|\ (_, b) v -> 1 + length b /= length v |])  (\_ v -> (undefined, replicate (length v - 1) undefined))
        , $(normal [| \s v -> True |] ) $
            $(update [p| (v:vs) |] [p| (v,vs) |] [d| v = Replace; vs = Replace |])
        ]
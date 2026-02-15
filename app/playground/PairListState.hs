{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

-- Explicit import lists to have a better idea of the origin of each part of
-- the example.

import Generics.BiGUL.AST
         ( Expr (EDir, EConst, EProd)
         , BiGUL (..)
         , RPat (RVar)
         , UPat (UVar, UProd)
         , Direction (DVar)
         , checkFullEmbed
         )
import Generics.BiGUL.Interpreter
         (put, get)
import Generics.BiGUL.MonadBiGULError
         ( MonadError' (..)
         , ErrorInfo
         )
import Generics.BiGUL.TH
         ( rearr
         , update
         )
-- import Control.Monad.Error.Class
import Control.Monad.Error.Class
import Control.Monad.State hiding (get, put)
import System.Exit (exitFailure)

instance MonadError' e m => MonadError' e (StateT s m) where
  -- catchBind :: StateT s m a -> (a -> StateT s m b) -> (e -> StateT s m b) -> StateT s m b
  catchBind ma f g = ((ma >>= return . Right) `catchError` (return . Left)) >>= either g f
  -- catchBind ma f g = (ma >>= f) `catchError` g
     -- !!! Not sure if this way it works correctly as it might catch exceptions
     -- in f !!!

-- |Main program showing how to use this BX.
main :: IO ()
main = do
  putStrLn "Source :: [(Int, Char)]"
  putStrLn "View :: [Int]"
  putStrLn ""
  putStr   "Checking..."
  b <- case evalStateT (checkFullEmbed pairlistcount) (0,0,0) of
    Left err -> putStrLn " [fail]" >> print err >> exitFailure
    Right b0 -> return b0
  if b then putStrLn " [ok]"
       else putStrLn " [fail]" >> exitFailure
  putStrLn ""
  putStrLn ("Original source: " ++ show s0)
  mapM_ (showPut pairlistcount) values
  where showPut bx (idx, s, v) = do
          putStrLn ""
          putStrLn ("----- " ++ show idx)
          putStrLn ("View:   " ++ show v)
          case runStateT (put bx s v) (0,0,0) of
            Left err -> putStrLn ("Error: " ++ show err)
            Right (s', c) -> putStrLn ("Source: " ++ show s' ++ "\n")
                          >> showCount c
        -- values
        values = [ (0::Int, s0, v0)
                 , (1, s0, v1)
                 , (2, s0, v2)
                 , (3, s0, v3)
                 , (4, s0, v4)
                 , (5, s0, v5)
                 , (6, s0, v6)
                 ]

showCount :: (Int, Int, Int) -> IO ()
showCount (t, a, d) = 
  putStrLn ("total: " ++ show t
           ++ " (added: " ++ show a
           ++ ", deleted: " ++ show d ++ ")")

-- Test values
s0 :: [(Int, Char)]
s0 = [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
v0, v1, v2, v3, v4, v5, v6 :: [Int]
v0 = [1,2,3,4,5]
v1 = [1,2,3]
v2 = [3,2,1]
v3 = [1,2,3,4,5,6,7,8]
v4 = [1,3,5]
v5 = [1,1,2,2,3,3,4,4,5,5]
v6 = [1,2,3,4,6]

-- |BX to update the key of a pair.
pairkey :: (MonadError' ErrorInfo m, Eq a) => BiGUL m (a, b) a
pairkey = $(rearr [| \ va -> (va, ()) |])
          $(update [p| (sa, _) |] [d| sa = Replace |])

-- |BX to align a list of pairs with keys.
pairlistcount
  :: MonadError' ErrorInfo m
  => BiGUL (StateT (Int,Int,Int) m) [(Int, Char)] [Int]
pairlistcount =
  Align
    -- source condition
    (\ _ -> return True)
    -- match
    (\ (sa, _) va -> return (sa == va))
    -- b
    (effect (modify incTotal) pairkey)
    -- create
    (\ va -> modify incAdd >> return (va, ' '))
    -- conceal
    (\ _ -> modify incDel >> return Nothing)

-- |Run an effect.
effect
  :: MonadError' ErrorInfo m
  => m ()
  -> BiGUL m s v
  -> BiGUL m s v
effect f b = Emb
  (\ s -> f >> get b s)
  (\ s v -> f >> put b s v)

-- *Auxiliary Functions

-- |Increment total count.
incTotal :: (Int, Int, Int) -> (Int, Int, Int)
incTotal (x, y, z) = (x+1, y, z)

-- |Increment additions.
incAdd :: (Int, Int, Int) -> (Int, Int, Int)
incAdd (x, y, z) = (x, y+1, z)

-- |Increment deletions.
incDel :: (Int, Int, Int) -> (Int, Int, Int)
incDel (x, y, z) = (x, y, z+1)

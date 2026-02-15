module Main where

-- Explicit import lists to have a better idea of the origin of each part of
-- the example.

import Generics.BiGUL.AST
         -- ( Expr (EDir, EConst, EProd)
         -- , BiGUL (..)
         -- , RPat (RVar)
         -- , UPat (UVar, UProd)
         -- , Direction (DVar)
         -- , checkFullEmbed
         -- )
import Generics.BiGUL.Interpreter
         (put)
import Generics.BiGUL.MonadBiGULError
         ( MonadError'
         , ErrorInfo
         )
import Generics.BiGUL.TH
         (update)

import System.Exit (exitFailure)

-- |Main program showing how to use this BX.
main :: IO ()
main = do
  putStrLn "Source :: [(Int, Char)]"
  putStrLn "View :: [Int]"
  putStrLn ""
  putStr   "Checking..."
  b <- case checkFullEmbed pairlist of
    Left err -> putStrLn " [fail]" >> print err >> exitFailure
    Right b0 -> return b0
  if b then putStrLn " [ok]"
       else putStrLn " [fail]" >> exitFailure
  putStrLn ""
  putStrLn ("Original source: " ++ show s0)
  -- 1 - simple version
  mapM_ (showPut pairlist) values
  -- 2 - version with substitution of elements
  putStrLn ""
  putStrLn "+++ With substitution of elements when possible"
  mapM_ (showPut pairlistsub) values
  where showPut bx (idx, s, v) = do
          putStrLn ""
          putStrLn ("----- " ++ show idx)
          putStrLn ("View:   " ++ show v)
          case put bx s v of
            Left err -> putStrLn ("Error: " ++ show err)
            Right s' -> putStrLn ("Source: " ++ show s')
        -- values
        values = [ (0::Int, s0, v0)
                 , (1, s0, v1)
                 , (2, s0, v2)
                 , (3, s0, v3)
                 , (4, s0, v4)
                 , (5, s0, v5)
                 , (6, s0, v6)
                 ]

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

-- |BX specification.
pairlist :: MonadError' ErrorInfo m => BiGUL m [(Int, Char)] [Int]
pairlist =
  Align
    -- source condition
    (\ _ -> return True)
    -- match
    (\ (sa, _) va -> return (sa == va))
    -- b
    ($(rearr [| \ va -> (va, ()) |])
        $(update [p| (sa, _) |]
                 [d| sa = Replace |]
          )
      )
    -- create
    (\ va -> return (va, ' '))
    -- conceal
    (\ _ -> return Nothing)

-- |BX specification with substitution of elements. (not possible at the moment)
--
-- The idea is, with v6 to have the 5 in the source modified to 6, keeping the
-- same data, which seems to not be possible at the moment.
pairlistsub :: MonadError' ErrorInfo m => BiGUL m [(Int, Char)] [Int]
pairlistsub =
  Align
    -- source condition
    (\ _ -> return True)
    -- match
    (\ (sa, _) va -> return (sa == va))
    -- b
    ($(rearr [| \ va -> (va, ()) |])
        $(update [p| (sa, _) |]
                 [d| sa = Replace |]
          )
      )
    -- create
    (\ va -> return (va, ' '))
    -- conceal
    (return . Just)

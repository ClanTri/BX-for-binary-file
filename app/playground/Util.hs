{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts  #-}
{- Utilities for simple testing by Zhenjiang Hu @ 22/09/2015 -}

module Util where
import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Error
import Control.Monad
import Control.Monad.Except

-- We prepare two simpler functions for testing put/get of
-- a bigul lens; it will give the result if it succeeds and
-- shows an error message if it fails.

testPut :: BiGUL s v -> s -> v -> PutResult s v
testPut = put

testGet :: BiGUL s v -> s -> GetResult s v
testGet = get


-- Composition is now a basic lenses.
-- Composition of two put lenses.
(@@) :: (Eq v) => BiGUL s x -> BiGUL x v -> BiGUL s v
u1 @@ u2 = emb getc putc
  where
    getc = fromEither . get u2 . fromEither . get u1
    putc s v = let x  = fromEither (get u1 s)
                   x' = fromEither (put u2 x v)
               in  fromEither (put u1 s x')
    fromEither :: (Show e) => Either e a -> a
    fromEither = either (error. show) id

emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \x y -> g x == y |])$
      $(rearrV [| \x -> ((), x) |])$
        Dep Skip (\x () -> g x)
  , $(adaptive [| \_ _ -> True |])
      p
  ]

-- We define a new lens to wrap Fail with additional error messages.
-- now Fail is a basic lens combinator
--failMsg :: String -> BiGUL s v
--failMsg msg = Emb getf putf
--  where
--    getf s = throwError $ ErrorInfo msg
--    putf s v = throwError $ ErrorInfo msg

-- Useful higher order lenses

mapU :: (Eq s, Eq v, Show v) => BiGUL s v -> BiGUL [s] [v]
mapU bigul =
  Case [ $(normalSV [p| [] |] [p| [] |]) $
           $(update [p| [] |] [p| [] |] [d| |])
       , $(normalSV [p| _:_ |] [p| _:_ |]) $
           $(update [p| x:xs |] [p| x:xs |] [d| x = bigul; xs = mapU bigul |])
       , $(adaptiveSV [p| _:_ |] [p| [] |]) $
           \_ _ -> []
       , $(adaptiveSV [p| [] |] [p| _:_ |]) $
           \_ _ -> [error "mapU"]
       ]

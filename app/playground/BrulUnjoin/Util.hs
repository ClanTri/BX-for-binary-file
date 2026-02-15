{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts  #-}

module Util where
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Control.Monad
import Control.Monad.Except

-- Composion of two put lenses.
(@@) :: BiGUL s x -> BiGUL x v -> BiGUL s v
u1 @@ u2 = emb getc putc
  where
    getc s = do x <- get u1 s
                get u2 x
    putc s v = do x <- get u1 s
                  x' <- put u2 x v
                  put u1 s x'

-- Useful higher order lenses
naiveMap :: (Show a, Show b) => BiGUL a b -> BiGUL [a] [b]
naiveMap b =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
                     [p| _:_ |])
          ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = naiveMap b |])
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
        , $(normalSV [p| [] |] [p| _:_ |]
                     [| const False |])
          ==> (Fail "length of the view should be less than that of the source.")
        , $(normalSV [p| [] |] [p| [] |]
                     [p| [] |])
          ==> $(update [p| [] |] [p| [] |] [d| |])
        ]


mapU :: (Eq s, Eq v, Show v) => BiGUL s v -> BiGUL [s] [v]
mapU bigul =
  Case [ $(normalSV [p| [] |] [p| [] |]) $
           $(rearrAndUpdate [p| [] |] [p| [] |] [d| |])
       , $(normalSV [p| _:_ |] [p| _:_ |]) $
           $(rearrAndUpdate [p| x:xs |] [p| x:xs |] [d| x = bigul; xs = mapU bigul |])
       , $(adaptiveSV [p| _:_ |] [p| [] |]) $
           \_ _ -> []
       , $(adaptiveSV [p| [] |] [p| _:_ |]) $
           \_ _ -> [error "mapU"]
       ]

emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> True |])
    ==> p
  ]
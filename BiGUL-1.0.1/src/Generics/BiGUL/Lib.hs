-- | A prelude for BiGUL programs.

module Generics.BiGUL.Lib where

import Generics.BiGUL
import Generics.BiGUL.TH


-- | Skip updating the source when the view is known to be a constant. Same as @Skip . const@.
skip :: Eq v => v -> BiGUL s v
skip v = Skip (const v)

-- | A nicer notation for applying a branch constructing function to a branch body. Same as 'Prelude.$'.
(==>) :: (a -> b) -> a -> b
(==>) = ($)

infixr 0 ==>

-- | Embed a well-behaved pair of transformations into BiGUL.
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> {- g s /= v -} True |])
    ==> p
  ]

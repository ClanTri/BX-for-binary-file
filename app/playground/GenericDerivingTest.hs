{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, TemplateHaskell, TupleSections  #-}
module GenericDerivingTest where

import GHC.Generics
--import GHC.InOut
import Language.Haskell.TH
import Control.Monad
import Generics.BiGUL.TH
import THAST

data Bookmark = Bookmark String String Bool
              | Folder String [Bookmark]
              | Sep
              deriving (Show)

deriveBiGULGeneric ''Bookmark

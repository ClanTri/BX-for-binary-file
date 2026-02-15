{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts, TupleSections, TemplateHaskell #-}

module TestPolymorphicData where
import Language.Haskell.TH
import GHC.Generics
import GHC.InOut
import Control.Monad
import Generics.BiGUL.TH
import Generics.BiGUL
import Generics.BiGUL.AST

-- data VBook b = VBook String b
--   deriving (Show)

-- instance Generic (VBook a) where
--   type Rep (VBook a) = (:*:) (K1 R String) (K1 R a)
--   from (VBook title var) = (:*:) (K1 title) (K1 var)
--   to ((:*:) (K1 title) (K1 var)) = VBook title var

-- qDecs = [d| instance Generic (VBook a_0) where 
--               type Rep (VBook a_0) = (:*:) (K1 R String) (K1 R a_0)
--               from (VBook var_1 var_2) = (:*:) (K1 var_1) (K1 var_2)
--               to ((:*:) (K1 var_1) (K1 var_2)) = VBook var_1 var_2 
--           |]

data VBook a b = VBook { title :: a, price :: b } deriving (Show)


deriveBiGULGeneric ''VBook

type SBook = (String, ([String], (Float, Int)))


s :: [SBook]
s = [("Real World Haskell is Not GOOD!", (["zantao"], (30.0, 2015)))]

-- v :: [VBook Float]
-- v = [VBook "Real World Haskell is Not GOOD!" 10.0,
--      VBook "Learn You Haskell is GOOD!" 20.0]

v :: [VBook String Float]
v = [VBook {title = "Real World Haskell is Not GOOD!", price = 10.0},
     VBook {title = "Learn You Haskell is GOOD!", price = 20.0}]

bookstore :: MonadError' ErrorInfo m => BiGUL m [SBook] [VBook String Float]
bookstore =
  Align (\_ -> return True)
        (\(stitle, _) (VBook vtitle _) -> return $ stitle == vtitle)
        ($(rearr [| \(VBook x y) -> (x, (), y, ()) |])
          -- Rearr (ROut (RProd RVar RVar))
          --       (EProd (EDir (DLeft DVar)) (EProd (EConst ()) (EProd (EDir (DRight DVar)) (EConst ()))))
               (Update (UProd (UVar Replace) (UProd (UVar Skip) (UProd (UVar Replace) (UVar Skip))))))
        (\_ -> return ("", ([], (0, 2012))))
        (\_ -> return Nothing)

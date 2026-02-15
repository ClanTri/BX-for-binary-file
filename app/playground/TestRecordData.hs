{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleContexts, TupleSections #-}

module TestRecordData where
import Language.Haskell.TH
import GHC.Generics
import GHC.InOut
import Control.Monad
import Generics.BiGUL.TH
import Generics.BiGUL
import Generics.BiGUL.AST


-- data Person = Person {
--   name:: String,
--   year:: Int
--   } 
--   deriving (Show)

-- deriveBiGULGeneric ''Person

-- data Selector_name

-- data Selector_year

-- instance Datatype Selector_name where
--       datatypeName _ = "Selector_name"
--       moduleName   _ = ""
-- instance Datatype Selector_year where
--       datatypeName _ = "Selector_year"
--       moduleName   _ = ""
-- instance Selector Selector_name where
--       selName _ = "name"
-- instance Selector Selector_year where
--       selName _ = "year"

-- instance Generic Person where
--       type Rep Person = (:*:) (S1 Selector_name (K1 R String)) (S1 Selector_year (K1 R Int))
--       from (Person var_afXm var_afXn) = (:*:) (M1 (K1 var_afXm)) (M1 (K1 var_afXn))
--       to ((:*:) (M1 (K1 var_afXm)) (M1 (K1 var_afXn))) = Person var_afXm var_afXn

--p1 = Person { name = "Zirun", year = 24 }


type SBook = (String, ([String], (Double, Int)))
data VBook = VBook {
    title :: String,
    price :: Double
  } | VBook1 String Double
  deriving (Show)

deriveBiGULGeneric ''VBook

s :: [SBook]
s = [("Real World Haskell is Not GOOD!", (["zantao"], (30.0, 2015)))]

v :: [VBook]
v = [VBook {title ="Real World Haskell is Not GOOD!", price = 10.0},
     VBook {title ="Learn You Haskell is GOOD!", price=20.0}]

data Bookmark = Bookmark String String Bool
              | Folder String [Bookmark]
              | Sep
              deriving (Show)


deriveBiGULGeneric ''Bookmark

bookstore :: MonadError' ErrorInfo m => BiGUL m [SBook] [VBook]
bookstore =
  Align (\_ -> return True)
        (\(stitle, _) (VBook vtitle _) -> return $ stitle == vtitle)
        ($(rearr [| \(VBook x y) -> (x, (), y, ()) |])
          -- Rearr (ROut (RProd RVar RVar))
          --       (EProd (EDir (DLeft DVar)) (EProd (EConst ()) (EProd (EDir (DRight DVar)) (EConst ()))))
               (Update (UProd (UVar Replace) (UProd (UVar Skip) (UProd (UVar Replace) (UVar Skip))))))
        (\_ -> return ("", ([], (0, 2012))))
        (\_ -> return Nothing)




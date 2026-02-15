{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies #-}

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import THAST

type SBook = (String, ([String], (Double, Int)))
data VBook = VBook String Double deriving (Show)

deriveBiGULGeneric ''VBook

s :: [SBook]
s = [("Real World Haskell is Not GOOD!", (["zantao"], (30.0, 2015)))]

v :: [VBook]
v = [VBook "Real World Haskell is Not GOOD!" 10.0,
     VBook "Learn You Haskell is GOOD!"      20.0]

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
        (\(VBook vtitle vprice) -> return (vtitle, ([], (vprice, 2012))))
        (\_ -> return Nothing)

{-
-- written in new syntax
-- 2015/09/24

bookstore =
	Align (\_ -> return True)
		  (\(stitle, _) (vtitle, _) -> return $ stitle == vtitle)
          ($(rearr [e| \(x, y) -> (x, (), y, ()) |])
          	$(update
          		[p| (title, _, price, _) |]
          		[d| title = Replace
          		    price = Replace |]
          		)
          	)
		  (\(vtitle, vprice) -> return (vtitle, ([], (vprice, 2012))))
		  (\_ -> return Nothing)
-}

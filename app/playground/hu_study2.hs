{- Studying notes by Zhenjiang Hu @ 23/09/2015
   This note is to show how new lenses can be defined in terms of
   a pair of get and put. Certainly we should avoid using Emb
   to introduce new lenses as their well-behavedness cannot be
   automatically guaranteed.
-}

import Generics.BiGUL.Interpreter
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Util


-- unGrouping:
-- [("x",1),("y",10),("y",11),("x",2),("y",12)] <-> [("x",[1,2,12]),("y",[10,11,12])]

unGrouping :: (Eq a, Eq b) => BiGUL [(a,b)] [(a,[b])]
unGrouping = emb get put
  where
    get [] = []
    get ((a,b):xs) = (a, b : [b' | (a',b') <- xs, a'==a])
                     : get [ (a',b') | (a',b') <- xs, a'/=a]
    put s [] = []
    put [] ((a,bs):vs) = [(a,b) | b<-bs] ++ put [] vs
    put ((a,b):ss) vs | a `elem` dom vs = let (b',vs') = split a vs
                                          in (a,b') : put ss vs'
                      | otherwise       = put ss vs
    dom vs = [ a | (a,_) <- vs ]
    split a (v@(a',b':bs'):vs) | a==a'     = if bs'==[] then (b',vs) else (b',(a',bs'):vs)
                               | otherwise = let (b'',vs') = split a vs
                                             in (b'',v:vs')

{-

*Main> testGet unGrouping [("x",1),("y",10),("y",11),("x",2),("y",12)]
Right [("x",[1,2]),("y",[10,11,12])]
*Main> testPut unGrouping [("x",1),("y",10),("y",11),("x",2),("y",12)] [("x",[1,2]),("y",[10,11,12])]
Right [("x",1),("y",10),("y",11),("x",2),("y",12)]
*Main> testPut unGrouping [("x",1),("y",10),("y",11),("x",2),("y",12)] [("x",[1,3]),("y",[10,11,12])]
Right [("x",1),("y",10),("y",11),("x",3),("y",12)]
*Main> testPut unGrouping [("x",1),("y",10),("y",11),("x",2),("y",12)] [("x",[1,3]),("y",[10,11])]
Right [("x",1),("y",10),("y",11),("x",3)]
*Main> testPut unGrouping [("x",1),("y",10),("y",11),("x",2),("y",12)] [("x",[1,3]),("y",[100,11])]
Right [("x",1),("y",100),("y",11),("x",3)]

-}

-- Another example is the lens composition (@@) as defined in Util.hs.


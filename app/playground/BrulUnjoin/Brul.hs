{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}
module Brul where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Data.List
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as Map

-- General datatypes for relational data
data RType = RInt Int 
           | RString String
           | RFloat Float
           | RDouble Double
      deriving (Show, Eq, Ord)
deriveBiGULGeneric ''RType



showRType :: RType -> String
showRType (RInt i) = show i
showRType (RString str) = str
showRType (RFloat f) = show f
showRType (RDouble d) = show d

tshow :: [Record] -> String
tshow [] = ""
tshow (line: ls) = tshow1 line ++ "\n" ++ tshow ls

tshow1 :: Record -> String
tshow1 [] = ""
tshow1 (r: rs) = showRType r ++ ", " ++ tshow1 rs

------------------------------------------------------------
showTable :: [Record] -> IO ()
showTable t = putStr (tshow t)

showResult (Right t) = showTable t
showResult (Left error) = putStrLn (show error)

showTuple :: ([Record], [Record]) -> IO ()
showTuple (s1, s2) = putStrLn "s1:" >>
                     showTable s1 >>
                     putStrLn "\ns2:" >>
                     showTable s2

showResultTuple (Right t) = showTuple t
showResultTuple (Left error) = putStrLn (show error)

type Record = [RType]

type Source = [Record]
type View = [Record]

-- A mapper to store the functional dependency
-- revise: many attributes may depend on the same attribute.
type FDMap = Map.Map Int [Int]

-- Mapping between source and view attribute, as brul does not support arithmetic operaiton yet, the correspondence is simply mapping.
type SVMap = Map.Map Int Int

-- update a specific location of a record with a value v with type RType.
uRecord :: Int -> RType -> Record -> Record
uRecord 0 v (x:xs) = v:xs
uRecord i v (x:xs) = x : uRecord (i-1) v xs  


-- find records in the view accordign to the specific source record attribute value. 
findWith :: Record -> Int -> [Record] -> Int -> Maybe Record
findWith rs is vs iv = 
    case find p vs of
      Just r  -> Just r
      Nothing -> Nothing
  where p :: Record -> Bool
        p vr = vr !! iv == rs !! is


type Brul s v = BiGUL s v

---- align operator for updating
align :: (Eq a, Show a, Eq b, Show b)
      => (a -> Bool)
      -> (a -> b -> Bool)
      -> Brul a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> (a -> a)  -- update functional dependency
      -> Brul [a] [b]
align p match b create conceal fd =  
  Case [ $(adaptiveSV [| \ss -> null (filter p ss) && map fd ss /= ss |] [p| [] |]) 
         ==> \ss _ -> map (\s1 -> let s2 = fd s1 in if p s2 then berror s1 else s2) ss    
       , $(adaptiveSV [| not . null . filter p |] [p| [] |])
         ==> \ss _ -> catMaybes (map (\s -> 
          let s1 = if p s then conceal s else Just s 
          in (maybe Nothing 
                (\s1 -> let s2 = fd s1 in if p s2 then berror s1 else (Just s2)) -- after fd, check p
                s1
             )) ss) -- using fd to update
       , $(adaptiveSV [| \ss -> not (null (filter p ss)) && not (p (head ss)) && (fd (head ss) /= head ss) |] [p| _:_ |])
         ==> \(s:xs) _ -> let s1 = fd s in if p s1 then berror s else s1: xs -- after fd, check p    
       , $(normalSV [| \ss -> null (filter p ss) |] [p| [] |] [| const True |])  
         ==> $(update [p| _ |] [p| [] |] [d| |])
       , $(normalSV [| \ss -> not (null (filter p ss)) && not (p (head ss)) |] [p| _:_ |] [| const True |])
         ==> $(update [p| _:vs |] [p| vs |] [d| vs = align p match b create conceal fd |])
       , $(normal [| \ss vs -> not (null (filter p ss)) && p (head ss) && not (null vs) && match (head ss) (head vs) |]
                  [| \ss -> not (null (filter p ss)) && p (head ss) |]) 
         ==> $(update [p| v: vs |] [p| v : vs |]
                      [d| v  = b
                          vs = align p match b create conceal fd |])
       , $(adaptiveSV [| const True |] [p| _:_ |])
         ==> \ss (v:_) -> case find (flip match v) (filter p ss) of
                          Nothing -> let s1 = fd (create v)
                                     in if p s1 
                                          then s1 :ss
                                          else berrorn s1
                          Just s  -> s :delete s ss ]

-- for showing error message.
berror s  = error $ "updated record according to functional dependency shall not satisfy p, related source record: " ++ show s
berrorn s = error $ "updated record according to functional dependency shall satisfy p, related source record: " ++ show s



------------------------------------------
-- computation of functional dependency

-- S, and V
fd :: FDMap -> FDMap -> SVMap -> View -> Record -> Record
fd sfdMap vfdMap svMap vs s = 
  let sfdList = Map.toAscList sfdMap
  in  fdHelper sfdList vfdMap svMap vs s

fdHelper :: [(Int, [Int])] -> FDMap -> SVMap -> View -> Record -> Record
fdHelper []                      vfdMap svMap vs s = s
fdHelper ((from, [to]): ms)      vfdMap svMap vs s = 
  case Map.lookup to svMap of
    Nothing  -> fdHelper ms vfdMap svMap vs s
    Just vto -> 
      case findVFrom vto vfdMap of
        Nothing    -> fdHelper ms vfdMap svMap vs s
        Just vfrom -> 
          case findWith s from vs vfrom of
            Nothing -> fdHelper ms vfdMap svMap vs s
            Just rv -> let s1 = uRecord to (rv !! vto) s
                       in fdHelper ms vfdMap svMap vs s1
fdHelper ((from, (to: tos)): ms)      vfdMap svMap vs s = 
  let s1 = fdHelper [(from, [to])] vfdMap svMap vs s
  in fdHelper ((from, tos): ms) vfdMap svMap vs s


findVFrom :: Int -> FDMap -> Maybe Int
findVFrom vto vfdMap = 
  let vfdList = Map.toAscList vfdMap
  in findVFromHelper vto vfdList

findVFromHelper :: Int -> [(Int, [Int])] -> Maybe Int
findVFromHelper vto [] = Nothing
findVFromHelper vto ((vfrom, vtos) : vs) = 
  case find (\v -> v == vto) vtos of
    Nothing -> findVFromHelper vto vs
    Just _  -> Just vfrom



-- |
-- > (==>) = ($)
-- make it more elegant to write ($). Later we may use (==>) instead of ($).
(==>) :: (a -> b) -> a -> b
(==>) = ($)

emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> True |])
    ==> p
  ]







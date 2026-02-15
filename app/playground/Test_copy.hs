{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns, ScopedTypeVariables  #-}

import Generics.BiGUL.Error
import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import GHC.Generics
--import qualified Netscape
--import qualified Xbel


main :: IO ()
main = putStrLn "Nothing to do: load the program into GHCi to test it."

---- iterative updates

iter :: Eq v => BiGUL s v -> BiGUL [s] v
iter b = Case
  [ ( $(normalS [p| [_] |])$
      $(update [p| v |] [p| v:_ |] [d| v = b |]) )
  , ( $(normalS [p| _:_ |])$
      $(rearrV [| \v -> (v, v) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` iter b )
  ]

iterBigul :: BiGUL [Int] Int
iterBigul = iter Replace

putIter :: [Int] -> Int -> Either (PutError [Int] Int) [Int]
putIter s v = put iterBigul s v

getIter :: [Int] -> Either (GetError [Int] Int) Int
getIter s = get iterBigul s

---- list alignment

align :: (a -> Bool)
      -> (a -> b -> Bool)
      -> BiGUL a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> BiGUL [a] [b]
align p match b create conceal = Case
  [ $(normalSV [| null . filter p |] [p| [] |])$
      $(rearrV [| \[] -> () |])$ Skip
  , $(adaptiveV [p| [] |])$
      \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)
  -- view is necessarily nonempty in the cases below
  , $(normalS [p| (p -> False):_ |])$
      $(rearrS [| \(s:ss) -> ss |])$
        align p match b create conceal
  , $(normal' [| \ss vs -> not (null ss) && p (head ss) && match (head ss) (head vs) |]
              [| \ss    -> not (null ss) && p (head ss) |])$
      $(rearrV [| \(v:vs) -> (v, vs) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` align p match b create conceal
  , $(adaptive [| \ss (v:_) -> isJust (findFirst (\s -> p s && match s v) ss) ||
                               let s = create v in p s && match s v |])$
      \ss (v:_) -> maybe (create v:ss) (uncurry (:)) (findFirst (\s -> p s && match s v) ss)
  ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findFirst p [] = Nothing
    findFirst p (x:xs) | p x       = Just (x, xs)
    findFirst p (x:xs) | otherwise = fmap (id *** (x:)) (findFirst p xs)

testAlign :: BiGUL [(Int, Char)] [Int]
testAlign = align (isUpper . snd)
                  (\(ks, _) v -> ks == v)
                  ($(update [p| v |] [p| (v, _) |] [d| v = Replace |]))
                  (\v -> (v, 'X'))
                  (\(k, c) -> Just (k, toLower c))

---- bookstore example

data SBook = SBook String [String] Double Int deriving (Show, Eq)
data VBook = VBook String Double deriving (Show, Eq)

deriveBiGULGeneric ''SBook
deriveBiGULGeneric ''VBook

s = [SBook "Real World Haskell is Not GOOD!" ["zantao"] 30.0 2015]
v = [VBook "Real World Haskell is Not GOOD!" 10.0, VBook "Learn You Haskell is GOOD!"  20.0]

bookstore :: BiGUL [SBook] [VBook]
bookstore =
  align (const True)
        (\(SBook stitle _ _ _) (VBook vtitle _) -> stitle == vtitle)
        ($(update [p| VBook title price |]
                  [p| SBook title _ price _ |]
                  [d| title = Replace
                      price = Replace |]))
        (\(VBook vtitle vprice) -> SBook vtitle [] vprice 2012)
        (const Nothing)

putBook :: Either (PutError [SBook] [VBook]) [SBook]
putBook = put bookstore s v

-- putBookWithCheck :: Either PutError [SBook]
-- putBookWithCheck = do
--   b <- checkFullEmbed bookstore
--   if b
--   then putBook
--   else throwError (BPFail "view variable(s) not fully embedded")

getBook :: Either (GetError [SBook] [VBook]) [VBook]
getBook = get bookstore s

-- checkBook :: Either PutError Bool
-- checkBook = checkFullEmbed bookstore

---- transatlantic corporation

type Name           = String
type Salary         = Float
type Location       = String
type Employee       = (Name, (Salary, Either Location Location))
type EmployeeSource = [Employee]


type EmployeeSimplified = (Name, Either Location Location)
type EmployeeView       = [EmployeeSimplified]

transatlantic :: BiGUL EmployeeSource EmployeeView
transatlantic =
  align (const True)
        (\(sName, _) (vName, _) -> sName == vName)
        (Replace `Prod`
         Case [ $(normalSV [p| (_, Left  _) |] [p| Left  _ |]) $
                  $(update [p| Left  loc |] [p| (_, Left  loc) |] [d| loc = Replace |])
              , $(normalSV [p| (_, Right _) |] [p| Right _ |]) $
                  $(update [p| Right loc |] [p| (_, Right loc) |] [d| loc = Replace |])
              , $(adaptiveSV [p| (_, Left  _) |] [p| Right _ |]) $
                  \(salary, _) loc -> (salary/3*5, loc)
              , $(adaptiveSV [p| (_, Right _) |] [p| Left  _ |]) $
                  \(salary, _) loc -> (salary/5*3, loc)
              ])
        (\(vName, location) -> (vName, (0, location)))
        (const Nothing)

employeeS :: EmployeeSource
employeeS = [ ("Jermy Gibbons", (82495, Left  "Oxford University" ))
            , ("Meng Wang"    , (13590, Left  "Oxford University" ))
            , ("Nate Foster"  , (97000, Right "Cornell University"))
            , ("Hugo Pacheco" , (35000, Right "Cornell University")) ]

getEmployee :: Either (GetError EmployeeSource EmployeeView) EmployeeView
getEmployee = get transatlantic employeeS

-- re-ordering
-- update location
-- deletion
-- insertion
employeeView' :: EmployeeView
employeeView' = [ ("Jermy Gibbons", Left  "Cambridge University")
                , ("Nate Foster"  , Left  "Oxford University"   )
                , ("Josh Ko"      , Left  "Oxford University"   )
                , ("Meng Wang"    , Right "Havard University"   ) ]

putEmployee :: Either (PutError EmployeeSource EmployeeView) EmployeeSource
putEmployee = put transatlantic employeeS employeeView'

---- view dependency

dep_pair :: BiGUL (Int, Int) (Int, Int)
dep_pair = Case
  [ $(normalV [| \(vx, vy) -> vx == vy |])$
      Case [ $(normalS [| (/= 0) . snd |])$
               Replace
           , $(normalS [p| _ |])$
               Dep ($(rearrV [| \x -> (x, 0) |])$ Replace)
                   (const id)
           ]
  , $(normalV [| (/= 0) . snd |])$
      Replace
  ]


---- trivial well-behaved wrapper

emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \x y -> g x == y |])$
      $(rearrV [| \x -> ((), x) |])$
        Dep Skip (\x () -> g x)
  , $(adaptive [| \_ _ -> True |])
      p
  ]

xforkS :: (s -> Bool) -> BiGUL [s] ([s], [s])
xforkS p = Case
  [ $(normalSV [p| [] |] [p| ([], []) |])$
      $(rearrV [| \([], []) -> () |])$
        Skip
  , $(normalSV [p| ((p -> True ):_) |] [p| (_:_, _) |])$
      $(rearrV [| \(x:xs, ys) -> (x, (xs, ys)) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          Replace `Prod` xforkS p
  , $(normalSV [p| ((p -> False):_) |] [p| (_, _:_) |])$
      $(rearrV [| \(xs, y:ys) -> (y, (xs, ys)) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          Replace `Prod` xforkS p
  , $(adaptiveSV [p| _ |] [p| _ |])$
      \_ (xs, ys) -> xs ++ ys
  ]

updateMultiple :: forall v s. Eq v => (v -> s -> Bool) -> BiGUL s v -> (v -> s) -> BiGUL [s] [v]
updateMultiple p b c = Case
  [ $(normalSV [p| [] |] [p| [] |])$
      $(update [p| [] |] [p| _ |] [d| |])
  , $(adaptiveV [p| [] |])$
      \_ _ -> []
  -- view is necessarily nonempty in the cases below
  , $(normal' [| \ss (v:_) -> firstMatched v ss && secondMatched v ss |]
              [| (>= 2) . length |])$
      $(rearrV [| \(v:vs) -> (v, v:vs) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` updateMultiple p b c
  , $(normal' [| \ss (v:_) -> firstMatched v ss && not (secondMatched v ss) |]
              [| not . null |])$
      $(update [p| v:vs |] [p| v:vs |] [d| v = b; vs = updateMultiple p b c |])
  , $(adaptive [| \ss (v:_) -> not (firstMatched v ss) |])$
      \ss (v:_) -> c v:ss
  ]
  where
    firstMatched :: v -> [s] -> Bool
    firstMatched v ((p v -> True):_) = True
    firstMatched v _                 = False
    secondMatched :: v -> [s] -> Bool
    secondMatched v (_:(p v -> True):_) = True
    secondMatched v _                   = False

updateMultiple' :: forall v s k. (Eq v, Eq k) => (v -> k) -> (s -> k) -> BiGUL s v -> (v -> s) -> BiGUL [s] [v]
updateMultiple' kv ks b c = Case
  [ $(normalSV [p| [] |] [p| [] |])$
      $(update [p| [] |] [p| _ |] [d| |])
  , $(adaptiveV [p| [] |])$
      \_ _ -> []
  -- view is necessarily nonempty in the cases below
  , $(normal' [| \ss (v:_) -> firstMatched v ss && secondMatched v ss |]
              [| \ss -> length ss >= 2 && let (s0:s1:_) = ss in ks s0 == ks s1 |])$
      $(rearrV [| \(v:vs) -> (v, v:vs) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` updateMultiple' kv ks b c
  , $(normal' [| \ss (v:_) -> firstMatched v ss && not (secondMatched v ss) |]
              [| not . null |])$
      $(update [p| v:vs |] [p| v:vs |] [d| v = b; vs = updateMultiple' kv ks b c |])
  , $(adaptive [| \ss (v:_) -> not (firstMatched v ss) |])$
      \ss (v:_) -> c v:ss
  ]
  where
    firstMatched :: v -> [s] -> Bool
    firstMatched v (s:_) | kv v == ks s = True
    firstMatched v _                   = False
    secondMatched :: v -> [s] -> Bool
    secondMatched v (_:s:_) | kv v == ks s = True
    secondMatched v _                     = False

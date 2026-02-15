{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import GHC.Generics
import Generics.BiGUL
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.List
import Control.Monad.Except

type Id = Int
type Name = String
type Age = Int
type Salary = Int
data Employee = Employee Id  Name Age Salary deriving(Show,Eq)
deriveBiGULGeneric ''Employee
data EmployeeV = EmployeeV Id Name Age deriving(Show,Eq)
deriveBiGULGeneric ''EmployeeV

projection ::  MonadError' ErrorInfo m => (Age -> Maybe Salary) -> Salary -> BiGUL m [Employee] [EmployeeV]
projection f d =
   Align (\_ -> return True)
         (\(Employee id1 _ _ _) (EmployeeV id2 _ _) -> return (id1 == id2))
         $(rearrAndUpdate [p| EmployeeV id name age  |]
                          [p| Employee  id name age _|]
                          [d| id = Replace ; name = Replace ; age = Replace |])
         (\(EmployeeV id name age) -> return $ case f age of
                                   Just salary -> Employee id name age salary
                                   Nothing -> Employee id name age d)
         (\_ -> return Nothing)

-- dependency Age -> Salary
computeDep :: [Employee] -> Age -> Maybe Salary
computeDep es = \agev -> case find (\(Employee _ _ ages _) -> ages == agev) es of
                            Just (Employee _ _ _ salary) -> Just salary
                            Nothing -> Nothing

projection' :: MonadError' ErrorInfo m => Salary -> BiGUL m [Employee] [EmployeeV]
projection' d = Emb (\s -> get (projection (computeDep []) d) s)
                    (\s v -> put (projection (computeDep s) d) s v)

projectionS :: [Employee]
projectionS = [Employee 1 "e1" 21 6000,
               Employee 2 "e2" 19 5500,
               Employee 3 "e3" 32 10000,
               Employee 4 "e4" 41 15000]

projectionV :: [EmployeeV]
projectionV = [EmployeeV 1 "e1a" 24,
               EmployeeV 3 "e3" 36,
               EmployeeV 5 "e5" 43,
               EmployeeV 6 "e6" 32]

{-
Parameterized bx
(projection (computeDep []) d) and (projection (computeDep s) d) are different bxs,
but we can proof they are well-behaved.

(projection (computeDep s) d) can be written in bigul , so it's well-behaved,let's denote it by b1
get-put: put b1 s (get b1 s) = s
put-get: get b1 (put b1 s v) = v

let's denote (projection (computeDep []) d) by b2
if we can proof (get b1) == (get b2)
then we have
get-put: put b1 s (get b2 s) = s
put-get: get b2 (put b1 s v) = v

proof of (get b1) == (get b2)
...

so (projection' d) is well-behaved.
-}


splitS :: MonadError' ErrorInfo m => (s -> m Bool) -> [s] -> m ([s],[s])
splitS ps [] = return ([],[])
splitS ps (x:xs) = do b <- ps x
                      (ls1,ls2) <- splitS ps xs
                      if b then return (x:ls1,ls2) else return (ls1,x:ls2)

{-
forkS :: MonadError' ErrorInfo m => (s -> m Bool) -> BiGUL m [s] v1 -> BiGUL m [s] v2 -> BiGUL m [s] (v1,v2)
forkS ps bigul1 bigul2 = Emb (\s -> do (s1,s2) <- splitS ps s
                                       v1 <- get bigul1 s1
                                       v2 <- get bigul2 s2
                                       return $ (v1,v2))
                             (\s (v1,v2) -> do (s1,s2) <- splitS ps s
                                               s1' <- put bigul1 s1 v1
                                               s2' <- put bigul2 s2 v2
                                               b1  <- mapM ps s1'
                                               b2  <- mapM ps s2'
                                               if and b1 && not (or b2)
                                               then return (s1'++s2')
                                               else throwError $ ErrorInfo "updated source violate condition")
-}

forkS :: MonadError' ErrorInfo m => (s -> m Bool) -> BiGUL m [s] v -> ([s] -> v -> m [s]) -> BiGUL m [s] v
forkS ps bigul f = Emb (\s -> do (s1,_) <- splitS ps s
                                 get bigul s1)
                       (\s v -> do (s1,s2) <- splitS ps s
                                   s1' <- put bigul s1 v
                                   s2' <- f s2 v
                                   return (s1'++s2'))


selection :: MonadError' ErrorInfo m => (Employee -> m Bool)
                                     -> BiGUL m [Employee] [Employee]
selection p = forkS p (Align p
                             (\(Employee id1 _ _ _) (Employee id2 _ _ _) -> return (id1 == id2))
                             Replace
                             return
                             (\(Employee id name age _) -> return $ Just (Employee id name age 0)))
                      (\ss vs -> computeDepf vs p ss)


-- dependency Name->Age
computeDepf :: MonadError' ErrorInfo m => [Employee] -> (Employee -> m Bool) -> [Employee] -> m [Employee]
computeDepf vs p ss = filterM (\s -> (notM (p s)) `andM` idNotInV s vs)
                                (map (\(Employee ids names ages salarys) -> case find (\(Employee _ namev _ _) -> names == namev) vs of
                                                                                 Just (Employee _ _ agev _) -> (Employee ids names agev salarys)
                                                                                 Nothing -> (Employee ids names ages salarys) ) ss)
                      where idNotInV (Employee ids _ _ _) vs =  case find (\(Employee idv _ _ _) -> ids == idv) vs of
                                                                          Just _ -> return False
                                                                          Nothing -> return True
                            notM mb = mb >>= \b -> return (not b)
                            andM mb1 mb2 = mb1 >>= \b1-> mb2 >>= \b2 -> return (b1 && b2)


selectionS :: [Employee]
selectionS = [Employee 1 "e1" 20 6000,
              Employee 2 "e2" 24 8000,
              Employee 3 "e3" 30 12000,
              Employee 5 "e5" 43 20000,
              Employee 7 "e7" 30 13000]

selectionV :: [Employee]
selectionV = [Employee 1 "e1" 25 12000,
              Employee 4 "e4" 32 15000,
              Employee 5 "e5" 44 23000,
              Employee 6 "e2" 30 16000]



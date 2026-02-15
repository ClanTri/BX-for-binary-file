-- | A higher-level syntax for programming in BiGUL, implemented in Template Haskell.

module Generics.BiGUL.TH (
  -- * 'GHC.Generics.Generic' instance derivation
    deriveBiGULGeneric
  -- * Rearrangement
  -- | * BiGUL does not support pattern matching for /n/-tuples where /n/ >= 3.
  --         For convenience (but possibly confusingly),
  --         the programmer __can__ use /n/-tuple patterns with the Template Haskell rearrangement syntax,
  --         but these patterns are translated into ones for right-nested pairs.
  --         For example, a 3-tuple pattern @(x, y, z)@ used in a rearrangement is in fact translated into @(x, (y, z))@.
  --
  --   * In a rearranging lambda-expression, if a pattern variable is used more than once in the body,
  --         the type of the pattern variable will be required to be an instance of 'Eq'.
  --
  --   * If an error message
  --
  --         > ‘C’ is not in the type environment at a reify
  --
  --         is reported where @C@ is a constructor used in a rearrangement,
  --         perhaps you forget to invoke 'deriveBiGULGeneric' on @C@’s datatype.
  , rearrS
  , rearrV
  , update
  -- * 'Case' branch construction
  -- | * In the following branch construction syntax, the meaning of a boolean-valued pattern-matching lambda-expression
  --         is redefined as a __total__ function which computes to 'False' when an input does not match the pattern;
  --         this meaning is different from that of a general pattern-matching lambda-expression, which fails to compute
  --         when the pattern is not matched. For example, in general the lambda-expression
  --
  --         > \(s:ss) (v:vs) -> s == v
  --
  --         will fail to compute if one of its inputs is an empty list; when used in branch construction, however,
  --         the lambda-expression will compute to 'False' upon encountering an empty list.
  --
  --   * An argument whose type is an instance of 'ExpOrPat' (a typeclass not exported) can be either
  --         a quoted expression (of type 'Language.Haskell.TH.Q' 'Language.Haskell.TH.Exp'),
  --         which should describe a unary or binary predicate (boolean-valued function), or a quoted pattern
  --         (of type 'Language.Haskell.TH.Q' 'Language.Haskell.TH.Pat'), which is translated into
  --         a unary predicate that computes to 'True' if the pattern is matched, or 'False' otherwise.
  , normal
  , normalSV
  , adaptive
  , adaptiveSV) where

import Data.Data
import Data.Maybe
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Extras (nameOfCon, namesBoundInPat)
import Generics.BiGUL
import Control.Monad


astNamespace :: String
astNamespace = "Generics.BiGUL"

data ConTag = L | R
  deriving (Show, Data, Typeable)

data PatTag = RTag   -- ^ view pattern
            | STag   -- ^ source pattern
            | ETag   -- ^ expression

instance Show PatTag where
   show ETag = "E"
   show _    = "P"

contag :: a -> a -> ConTag -> a
contag x _ L = x
contag _ y R = y

type Namespace        = String
type TypeConstructor  = String
type ValueConstructor = String
type ErrorMessage     = String

lookupName :: (String -> Q (Maybe Name)) -> ErrorMessage -> String -> Q Name
lookupName f errMsg name = f name >>= maybe (fail errMsg) return

lookupNames :: Namespace -> [TypeConstructor] -> [ValueConstructor] -> Q ([Name], [Name])
lookupNames namespace typeCList valueCList =
  let qualifiedName c = namespace ++ "." ++ c
      errorMessage  c = "‘" ++ c ++ "’ is not in scope (perhaps you forget to import " ++ namespace ++ ")"
  in  liftM2 (,) (mapM (\c -> lookupName lookupTypeName  (errorMessage c) (qualifiedName c)) typeCList )
                 (mapM (\c -> lookupName lookupValueName (errorMessage c) (qualifiedName c)) valueCList)

-- | Generate a 'GHC.Generics.Generic' instance for a named datatype
--   so that its constructors can be used in rearranging lambda-expressions.
--   Invoke this function on a datatype @T@ by putting
--
--   > deriveBiGULGeneric ''T
--
--   at the top level of a source file (say, after the definition of @T@).
--   Only simple datatypes and newtypes are supported (no GADTs, for example);
--   type parameters and named fields (record syntax) are supported.
deriveBiGULGeneric :: Name -> Q [InstanceDec]
deriveBiGULGeneric name = do
  (name, typeVars, constructors) <-
    do
      info <- reify name
      case info of
#if __GLASGOW_HASKELL__ >= 800
        (TyConI (DataD [] name typeVars _ constructors _)) ->
#else
        (TyConI (DataD [] name typeVars constructors _)) ->
#endif
          return (name, typeVars, constructors)
#if __GLASGOW_HASKELL__ >= 800
        (TyConI (NewtypeD [] name typeVars _ constructor _)) ->
#else
        (TyConI (NewtypeD [] name typeVars constructor _)) ->
#endif
          return (name, typeVars, [constructor])
        _ -> fail ("‘" ++ nameBase name ++ "’ is not in scope or not a (supported) datatype")
  ([nGeneric, nRep, nK1, nR, nU1, nSum, nProd, nV1, nS1, nSelector, nDataType],
   [vFrom, vTo, vK1, vL1, vR1, vU1, vProd, vSelName, vDataTypeName, vModuleName, vM1]) <-
    lookupNames "GHC.Generics"
                ["Generic", "Rep", "K1", "R", "U1", ":+:", ":*:", "V1", "S1", "Selector", "Datatype"]
                ["from", "to", "K1", "L1", "R1", "U1", ":*:", "selName", "datatypeName", "moduleName", "M1"]
  env <- consToEnv constructors
  selectorsNameList <- generateSelectorNames constructors
  let selectorDataDMaybeList = generateSelectorDataD selectorsNameList
  let selectorDataTypeMaybeList =
        map (generateSelectorDataType nDataType vDataTypeName vModuleName (maybe "" id (nameModule name)))
            selectorsNameList
  let selectorNameAndConList = zip selectorsNameList constructors
  let selectorInstanceDecList = map (generateSelectorInstanceDec nSelector vSelName) selectorNameAndConList
  let fromClauses = map (constructFuncFromClause (vK1, vU1, vL1, vR1, vProd, vM1)) env
  let toClauses   = map (constructFuncToClause (vK1, vU1, vL1, vR1, vProd, vM1)) env
  return $ catMaybes selectorDataDMaybeList ++
           catMaybes (concat selectorDataTypeMaybeList) ++
           catMaybes (concat selectorInstanceDecList) ++
           [InstanceD
#if __GLASGOW_HASKELL__ >= 800
              Nothing
#endif
              []
              (AppT (ConT nGeneric) (generateTypeVarsType name typeVars))
              [TySynInstD nRep
                 (TySynEqn
                    [generateTypeVarsType name typeVars]
                    (constructorsToSum (nSum, nV1)
                       (map (constructorToProduct (nK1, nR, nU1, nProd, nS1)) selectorNameAndConList))),
               FunD vFrom fromClauses,
               FunD vTo toClauses]
            ]

constructorsToSum :: (Name, Name) -> [Type] -> Type
constructorsToSum (sum, v1) []  = ConT v1
constructorsToSum (sum, v1) tps = foldr1 (\t1 t2 -> (ConT sum `AppT` t1) `AppT` t2) tps

constructorToProduct :: (Name, Name, Name, Name, Name) -> ([Maybe Name], Con) -> Type
constructorToProduct (k1, r, u1, prod, s1) (_,     NormalC _ [] ) = ConT u1
constructorToProduct (k1, r, u1, prod, s1) (_,     NormalC _ sts) =
  foldr1 (\t1 t2 -> (ConT prod `AppT` t1 ) `AppT` t2) (map (AppT (ConT k1 `AppT` ConT r) . snd) sts)
constructorToProduct (k1, r, u1, prod, s1) (names, RecC    _ sts) =
  foldr1 (\t1 t2 -> (ConT prod `AppT` t1 ) `AppT` t2)
    (map (\(Just n, (_,_,t)) -> AppT (ConT s1 `AppT` ConT n) ((ConT k1 `AppT` ConT r) `AppT` t)) (zip names sts))
constructorToProduct _                     (_,     c)             =
  error ("Constructor ‘" ++ nameBase (nameOfCon c) ++ "’ is of an unsupported kind")

-- Bool indicates: if Normal then False else RecC True
constructorToPatAndBody :: Con -> Q (Bool, Name, [Name])
constructorToPatAndBody (NormalC name sts) = liftM (False, name,) (replicateM (length sts) (newName "var"))
constructorToPatAndBody (RecC    name sts) = liftM (True , name,) (replicateM (length sts) (newName "var"))
constructorToPatAndBody c                  =
  fail ("Constructor ‘" ++ nameBase (nameOfCon c) ++ "’ is of an unsupported kind")

zipWithLRs :: [(Bool, Name, [Name])] ->  [(Bool, Name, [ConTag], [Name])]
zipWithLRs nns = zipWith (\(b, n, ns) lrs -> (b, n, lrs, ns)) nns (constructLRs (length nns))

consToEnv :: [Con] -> Q [(Bool, Name, [ConTag], [Name])]
consToEnv cons = liftM zipWithLRs (mapM constructorToPatAndBody cons)

constructFuncFromClause :: (Name, Name, Name, Name, Name, Name) -> (Bool, Name, [ConTag], [Name]) -> Clause
constructFuncFromClause (vK1, vU1, vL1, vR1, vProd, vM1) (b, n, lrs, names) =
  Clause [ConP n (map VarP names)] (NormalB (wrapLRs lrs (deriveGeneric names))) []
  where
    wrapLRs :: [ConTag] -> Exp -> Exp
    wrapLRs lrs exp = foldr (\lr e -> ConE (contag vL1 vR1 lr) `AppE` e) exp lrs

    deriveGeneric :: [Name] -> Exp
    deriveGeneric []    = ConE vU1
    deriveGeneric names = foldr1 (\e1 e2 -> (ConE vProd `AppE` e1) `AppE` e2)
                            (map (\name -> if b then ConE vM1 `AppE` (ConE vK1 `AppE` VarE name)
                                                else ConE vK1 `AppE` VarE name) names)

constructFuncToClause :: (Name, Name, Name, Name, Name, Name) -> (Bool, Name, [ConTag], [Name])  -> Clause
constructFuncToClause (vK1, vU1, vL1, vR1, vProd, vM1) (b, n, lrs, names) =
  Clause [wrapLRs lrs (deriveGeneric names)] (NormalB (foldl (\e1 name -> e1 `AppE` (VarE name)) (ConE n) names) ) []
  where
    wrapLRs :: [ConTag] -> TH.Pat -> TH.Pat
    wrapLRs lrs pat = foldr (\lr p -> ConP (contag vL1 vR1 lr) [p]) pat lrs

    deriveGeneric :: [Name] -> TH.Pat
    deriveGeneric []    = ConP vU1 []
    deriveGeneric names = foldr1 (\p1 p2 -> ConP vProd [p1, p2])
                            (map (\name -> if b then (ConP vM1 ((:[]) (ConP vK1 ((:[]) (VarP name)))))
                                                else (ConP vK1 ((:[]) (VarP name)))) names)

-- construct selector names from constructors
generateSelectorNames :: [Con] -> Q [[Maybe Name]]
generateSelectorNames = mapM (\con ->
  case con of {
      RecC _ sts -> mapM (\(n, _, _) -> newName ("Selector_" ++ nameBase n) >>= return . Just) sts;
      _          -> return []
    })

generateSelectorDataD :: [[Maybe Name]] -> [Maybe Dec]
generateSelectorDataD names =
#if __GLASGOW_HASKELL__ >= 800
  map (fmap (\n -> DataD [] n [] Nothing [] [])) (concat names)
#else
  map (fmap (\n -> DataD [] n [] [] [])) (concat names)
#endif

-- Selector DataType Generation
generateSelectorDataType :: Name -> Name -> Name -> String -> [Maybe Name] -> [Maybe Dec]
generateSelectorDataType nDataType vDataTypeName vModuleName moduleName =
  map (generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName)

generateSelectorDataType' :: Name -> Name -> Name -> String -> Maybe Name -> Maybe Dec
generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName (Just selectorName) =
  Just $
    InstanceD
#if __GLASGOW_HASKELL__ >= 800
      Nothing
#endif
      []
      (AppT (ConT nDataType) (ConT selectorName))
      [FunD vDataTypeName ([Clause [WildP] (NormalB (LitE (StringL (show selectorName)))) []]),
       FunD vModuleName   ([Clause [WildP] (NormalB (LitE (StringL moduleName))) []])
      ]
generateSelectorDataType' nDataType vDataTypeName vModuleName moduleName _ = Nothing

-- Selector Instance Declaration generation
generateSelectorInstanceDec :: Name -> Name -> ([Maybe Name], Con) -> [Maybe Dec]
generateSelectorInstanceDec nSelector vSelName ([]   , _  ) = []
generateSelectorInstanceDec nSelector vSelName (names, (RecC _ sts)) = map (generateSelectorInstanceDec' nSelector vSelName) (zip names sts)

generateSelectorInstanceDec' :: Name -> Name -> (Maybe Name, THS.VarStrictType) -> Maybe Dec
generateSelectorInstanceDec' nSelector vSelName (Just selectorName, (name, _, _)) =
  Just $
    InstanceD
#if __GLASGOW_HASKELL__ >= 800
      Nothing
#endif
      []
      (AppT (ConT nSelector) (ConT selectorName))
      [FunD vSelName ([Clause [WildP] (NormalB (LitE (StringL (nameBase name)))) []])]
generateSelectorInstanceDec' _         _         _                          = Nothing

-- generate type representation of polymorhpic type
-- e.g. VBook a b is represented as: AppT (ConT name) (ConT name_a `AppT` ConT name_b)
generateTypeVarsType :: Name -> [TyVarBndr] -> Type
generateTypeVarsType n []    = ConT n -- not polymorphic case.
generateTypeVarsType n tvars = foldl (\a b -> AppT a b) (ConT n) $ map (\tvar ->
   case tvar of
    { PlainTV  name      -> VarT name;
      KindedTV name kind -> VarT name -- error "kind type variables are not supported yet."
    }) tvars

constructLRs :: Int -> [[ConTag]]
constructLRs 0 = []
constructLRs 1 = [[]]
constructLRs n = [L] : map (R:) (constructLRs (n-1))

lookupLRs :: Name -> Q [ConTag]
lookupLRs conName = do
  info <- reify conName
  datatypeName <-
    case info of
#if __GLASGOW_HASKELL__ >= 800
      DataConI _ _ n -> return n
#else
      DataConI _ _ n _ -> return n
#endif
      _ -> fail $ "‘" ++ nameBase conName ++ "’ is not a data constructor"
  typeInfo <- reify datatypeName
  let cons = case typeInfo of
#if __GLASGOW_HASKELL__ >= 800
               TyConI (DataD _ _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ _ con _) -> [con]
#else
               TyConI (DataD _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ con _) -> [con]
#endif
               _                             -> []
  return $ constructLRs (length cons) !!
             fromJust (List.findIndex (== conName) (map (\con -> case con of { NormalC n _ -> n; RecC n _ -> n}) cons))

lookupRecordLength :: Name -> Q Int
lookupRecordLength conName = do
  info <- reify conName
  datatypeName <-
    case info of
#if __GLASGOW_HASKELL__ >= 800
      DataConI _ _ n -> return n
#else
      DataConI _ _ n _ -> return n
#endif
      _ -> fail $ "‘" ++ nameBase conName ++ "’ is not a data constructor"
  typeInfo <- reify datatypeName
  let cons = case typeInfo of
#if __GLASGOW_HASKELL__ >= 800
               TyConI (DataD _ _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ _ con _) -> [con]
#else
               TyConI (DataD _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ con _) -> [con]
#endif
               _                             -> []
  return $ (\(RecC _ fs) -> length fs) (fromJust (List.find (\(RecC n _) -> n == conName) cons))

lookupRecordField :: Name -> Name -> Q Int
lookupRecordField conName fieldName = do
  info <- reify conName
  datatypeName <-
    case info of
#if __GLASGOW_HASKELL__ >= 800
      DataConI _ _ n -> return n
#else
      DataConI _ _ n _ -> return n
#endif
      _ -> fail $ "‘" ++ nameBase conName ++ "’ is not a data constructor"
  typeInfo <- reify datatypeName
  let cons = case typeInfo of
#if __GLASGOW_HASKELL__ >= 800
               TyConI (DataD _ _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ _ con _) -> [con]
#else
               TyConI (DataD _ _ _ cons _)   -> cons
               TyConI (NewtypeD _ _ _ con _) -> [con]
#endif
               _                             -> []
  case (List.findIndex (\(n,_,_) -> n == fieldName) ((\(RecC _ fs) -> fs) $ fromJust (List.find (\(RecC n _) -> n == conName) cons))) of
       Just res -> return res
       Nothing -> fail $ "‘" ++ nameBase fieldName ++ "’ is not a field in ‘" ++ nameBase conName ++ "’"

mkConstrutorFromLRs :: [ConTag] -> PatTag -> Q (Exp -> Exp)
mkConstrutorFromLRs lrs patTag = do
  (_, [gin, gleft, gright]) <- lookupNames astNamespace [] (map (show patTag ++) ["In", "Left", "Right"])
  return (foldl (.) (AppE (ConE gin)) (map (AppE . ConE . contag gleft gright) lrs))

mkPat :: TH.Pat -> PatTag -> [Name] -> Q TH.Exp

mkPat (LitP c) patTag _ = do
  (_, [gconst]) <- lookupNames astNamespace [] [show patTag ++ "Const"]
  return $ ConE gconst `AppE` LitE c

-- user defined datatypes && unit pattern
mkPat (ConP name ps) patTag dupnames = do
  ConP name' [] <- [p| () |]
  if name == name' && ps == []
  then do
       unitt         <- [| () |]
       (_, [gconst]) <- lookupNames astNamespace [] [show patTag ++ "Const"]
       return $ ConE gconst `AppE` unitt
  else do
       lrs <- lookupLRs name
       conInEither <- mkConstrutorFromLRs lrs patTag
       pes         <- case ps of
                       [] -> mkPat (ConP name' []) patTag dupnames
                       _  -> mkPat (TupP ps)  patTag dupnames
       return $ conInEither pes

mkPat (RecP name ps) patTag dupnames = do
  -- reduce the case for a record constructor to the case for an ordinary constructor
  len <- lookupRecordLength name -- number of constructor arguments
  indexs <- mapM (\(n,_) -> lookupRecordField name n) ps -- positions of the fields mentioned in p
  let nps = map snd ps  -- patterns for the fields
  mkPat (ConP name (helper 0 len (zip indexs nps) [])) patTag dupnames -- grab the pattern for position i for each 0 <= i < len from zip indexs nps
  where findInPair [] i = WildP
        findInPair ((j,p):xs) i | i == j = p
                                | otherwise = findInPair xs i
        helper i n pairs acc  | i == n = acc
                              | otherwise = helper (i+1) n pairs (acc++[findInPair pairs i])
        -- let ips = zip indexs nps in [ maybe WildP id (List.lookup i ips) | i <- [0..len-1] ]

mkPat (ListP []) patTag dupnames = do emptyp <- [p| [] |]
                                      mkPat emptyp patTag dupnames

mkPat (ListP (p:xs)) patTag dupnames = do
  hexp <- mkPat p patTag dupnames
  rexp <- mkPat (ListP xs) patTag dupnames
  (_, [gin,gright,gprod]) <- lookupNames astNamespace [] (map (show patTag ++) ["In", "Right", "Prod"])
  return $ ConE gin `AppE` (ConE gright `AppE` (ConE gprod `AppE` hexp `AppE` rexp))

mkPat (InfixP pl name pr) patTag dupnames = do
  ConE name' <- [| (:) |]
  if name == name'
  then do lpat <- mkPat pl patTag dupnames
          rpat <- mkPat pr patTag dupnames
          (_, [gin,gright,gprod]) <- lookupNames astNamespace [] (map (show patTag ++) ["In", "Right", "Prod"])
          return $ ConE gin `AppE` (ConE gright `AppE` (ConE gprod `AppE` lpat `AppE` rpat))
  else fail $ "Infix use of ‘" ++ nameBase name ++ "’ is not supported"

mkPat (TupP [p]) patTag dupnames = mkPat p patTag dupnames

mkPat (TupP (p:ps)) patTag dupnames = do
  lexp <- mkPat p patTag dupnames
  rexp <- mkPat (TupP ps) patTag dupnames
  (_, [gprod]) <- lookupNames astNamespace [] [show patTag ++ "Prod"]
  return ((ConE gprod `AppE` lexp) `AppE` rexp)

mkPat (WildP) RTag _ = fail $ "Wildcard (‘_’) is forbidden in a view-rearranging pattern"

mkPat (WildP) STag _ = do
  (_, [pvar'])       <- lookupNames astNamespace [] ["PVar'"]
  return $ ConE pvar'

mkPat (VarP name) _  dupnames =  do
  (_, [pvar,pvar'])       <- lookupNames astNamespace [] ["PVar", "PVar'"]
  return $ if name `elem` dupnames then ConE pvar else ConE pvar'

mkPat _ patTag _ = fail "Unsupported pattern in a rearranging lambda-expression"


-- | translate all (VarE name) to directions using env
rearrangeExp :: Exp -> Map String Exp -> Q Exp
rearrangeExp (VarE name)  env =
  case Map.lookup (nameBase name) env of
    Just val -> return val
    Nothing  -> fail $ "Panic: Unbound variable ‘" ++ nameBase name ++ "’"
rearrangeExp (AppE e1 e2) env = liftM2 AppE (rearrangeExp e1 env) (rearrangeExp e2 env)
rearrangeExp (ConE name)  env = return $ ConE name
rearrangeExp (LitE c)     env = return $ LitE c
rearrangeExp _            env = fail "Unsupported expression in a rearranging lambda-expression"

mkEnvForRearr :: TH.Pat -> Q (Map String Exp)
mkEnvForRearr (LitP c) = return Map.empty

-- empty list is ok , mkEnvForRearr return Q Map.empty for it
mkEnvForRearr (ConP name ps) = mkEnvForRearr (TupP ps)

mkEnvForRearr (RecP name ps) = do
  len <- lookupRecordLength name
  indexs <- mapM (\(n,_) -> lookupRecordField name n) ps
  let nps = map snd ps
  mkEnvForRearr (ConP name (helper 0 len (zip indexs nps) []))
  where findInPair [] i = WildP
        findInPair ((j,p):xs) i | i == j = p
                                | otherwise = findInPair xs i
        helper i n pairs acc  | i == n = acc
                              | otherwise = helper (i+1) n pairs (acc++[findInPair pairs i])

mkEnvForRearr (ListP []) = return Map.empty
mkEnvForRearr (ListP (pl:pr))     = do
  (_, [dleft,dright]) <- lookupNames astNamespace [] ["DLeft", "DRight"]
  lenv <- mkEnvForRearr pl
  renv <- mkEnvForRearr (ListP pr)
  return $ Map.map (ConE dleft `AppE`) lenv `Map.union`
          Map.map (ConE dright `AppE`) renv

mkEnvForRearr (InfixP pl name pr) = do
  (_, [dleft,dright]) <- lookupNames astNamespace [] ["DLeft", "DRight"]
  lenv <- mkEnvForRearr pl
  renv <- mkEnvForRearr pr
  return $ Map.map (ConE dleft  `AppE`) lenv `Map.union`
           Map.map (ConE dright `AppE`) renv

mkEnvForRearr (TupP ps) = do
  (_, [dleft,dright]) <- lookupNames astNamespace [] ["DLeft", "DRight"]
  subenvs             <- mapM mkEnvForRearr ps
  let envs            =  zipWith (Map.map . foldr (.) id . map (AppE . ConE . contag dleft dright))
                                 (constructLRs (length ps)) subenvs
  return $ Map.unions envs

mkEnvForRearr WildP = return Map.empty

mkEnvForRearr (VarP name) = do
  (_, [dvar]) <- lookupNames astNamespace [] ["DVar"]
  return $ Map.singleton (nameBase name) (ConE dvar)

mkEnvForRearr  _    =  fail "Unsupported pattern in a rearranging lambda-expression"


splitDataAndCon:: TH.Exp -> Q (TH.Exp -> TH.Exp ,[TH.Exp])

splitDataAndCon (AppE (ConE name) e2) = do
  lrs <- lookupLRs name
  con <- mkConstrutorFromLRs lrs ETag
  d   <- mkBodyExpForRearr e2
  return (con,[d])

splitDataAndCon (AppE e1 e2) = do
  (c, ds) <- splitDataAndCon e1
  d        <- mkBodyExpForRearr e2
  return (c,ds++[d])

splitDataAndCon _            =  fail "Invalid data constructor in a rearranging lambda-expression"


mkBodyExpForRearr :: TH.Exp -> Q TH.Exp

mkBodyExpForRearr (LitE c) = do
  (_, [econst]) <- lookupNames astNamespace [] ["EConst"]
  return $ ConE econst `AppE` (LitE c)

mkBodyExpForRearr (VarE name) =  return $ VarE name

mkBodyExpForRearr (AppE e1 e2) = do
  -- must be constructor applied to arguments (rearrangement expression does not allow general functions)
  -- surface syntax is curried constructor applied to arguments in order; should translate that to uncurried constructor applied to a tuple of arguments
  (_, [eprod]) <- lookupNames astNamespace [] ["EProd"]
  (con, ds)   <- splitDataAndCon (AppE e1 e2)
  return $ con (foldr1 (\d1 d2 -> ConE eprod `AppE` d1 `AppE` d2) ds)

mkBodyExpForRearr (ConE name) =  do
  -- must be constructor without argument
  (ConE name') <- [| () |]
  (_, [econst]) <- lookupNames astNamespace [] ["EConst"]
  if name == name'
  then return $ ConE econst `AppE` (ConE name)
  else mkBodyExpForRearr (AppE (ConE name) (ConE name'))

mkBodyExpForRearr (RecConE name es) = do
  -- reduce to the case for ordinary constructors
  (ConE name') <- [| () |]
  (_, [econst,eprod]) <- lookupNames astNamespace [] ["EConst", "EProd"]
  len <- lookupRecordLength name
  indexs <- mapM (\(n,_) -> lookupRecordField name n) es
  let nes =  map snd es
  mkBodyExpForRearr (foldl (\acc e -> acc `AppE` e) (ConE name) (helper 0 len (zip indexs nes) [] (ConE name')))
  where findInPair [] i  unit = unit
        findInPair ((j,p):xs) i unit | i == j = p
                                     | otherwise = findInPair xs i unit
        helper i n pairs acc unit | i == n = acc
                                  | otherwise = helper (i+1) n pairs (acc ++[(findInPair pairs i unit)]) unit

-- restrict infix op to : for now
mkBodyExpForRearr (InfixE (Just e1) (ConE name) (Just e2)) = do
  (ConE name') <- [| (:) |]
  if name == name'
  then do le <- mkBodyExpForRearr e1
          re <- mkBodyExpForRearr e2
          (_, [ein,eright,eprod]) <- lookupNames astNamespace [] ["EIn", "ERight", "EProd"]
          return $ ConE ein `AppE` (ConE eright `AppE` (ConE eprod `AppE` le `AppE` re))
  else fail $ "Infix use of ‘" ++ nameBase name ++ "’ is not supported"

mkBodyExpForRearr (ListE [])  = do
  unitt                   <- [| () |]
  (_, [ein,eleft,econst]) <- lookupNames astNamespace [] ["EIn", "ELeft", "EConst"]
  return $ ConE ein `AppE` (ConE eleft `AppE` (ConE econst `AppE` unitt))
mkBodyExpForRearr (ListE (e:es)) = do
  hexp <- mkBodyExpForRearr e
  rexp <- mkBodyExpForRearr (ListE es)
  (_, [ein,eright,eprod]) <- lookupNames astNamespace [] ["EIn", "ERight", "EProd"]
  return $ ConE ein `AppE` (ConE eright `AppE` (ConE eprod `AppE` hexp `AppE` rexp))

mkBodyExpForRearr (TupE [e])    = mkBodyExpForRearr e
mkBodyExpForRearr (TupE (e:es)) = do
  lexp <- mkBodyExpForRearr e
  rexp <- mkBodyExpForRearr (TupE es)
  (_, [eprod]) <- lookupNames astNamespace [] ["EProd"]
  return ((ConE eprod `AppE` lexp) `AppE` rexp)
mkBodyExpForRearr _           = fail "Unsupported expression in a rearranging lambda-expression"


rearr' :: PatTag -> TH.Exp -> [Name] -> Q TH.Exp
rearr' patTag (LamE [p] e) dupnames = do
  let suffixRS = case patTag of {RTag -> "V" ; STag -> "S" ; _ -> ""}
  (_, [edir,rearrc]) <- lookupNames astNamespace [] ["EDir", "Rearr" ++ suffixRS]
  pat <- mkPat p patTag dupnames
  exp <- mkBodyExpForRearr e
  env <- mkEnvForRearr p
  newexp <- rearrangeExp exp (Map.map (ConE edir `AppE`) env)
  return ((ConE rearrc `AppE` pat) `AppE` newexp)

getAllVars :: TH.Exp -> [Name]
getAllVars (LitE c) = []
getAllVars (VarE name) = [name]
getAllVars (AppE e1 e2) = getAllVars e1 ++ getAllVars e2
getAllVars (ConE name) = []
getAllVars (RecConE name es) = concatMap getAllVars (map snd es)
getAllVars (InfixE (Just e1) (ConE name) (Just e2)) = getAllVars e1 ++ getAllVars e2
getAllVars (ListE es) = concatMap getAllVars es
getAllVars (TupE  es) = concatMap getAllVars es
getAllVars  _         =  fail "Unsupported expression in a rearranging lambda-expression"

-- | A higher-level syntax for 'Generics.BiGUL.RearrS',
--   allowing its first and second arguments to be specified in terms of a simple lambda-expression.
--   The usual way of using 'rearrS' is
--
--   > $(rearrS [| f |]) b :: BiGUL s v
--
--   where @f :: s -> s'@ is a simple lambda-expression and @b :: BiGUL s' v@ an inner program.
rearrS :: Q TH.Exp  -- ^ rearranging lambda-expression
       -> Q TH.Exp
rearrS qlambexp = do
  lambexp <- qlambexp
  case lambexp of
    LamE [_] e ->
      let varnames = getAllVars e
      in  rearr' STag lambexp (varnames \\ nub varnames)
    LamE _   _ ->
      fail "A rearranging lambda-expression should have exactly one argument"
    _          ->
      fail "The first argument to rearrS should be a (quoted) lambda-expression"

-- | A higher-level syntax for 'Generics.BiGUL.RearrV',
--   allowing its first and second arguments to be specified in terms of a simple lambda-expression.
--   The usual way of using 'rearrV' is
--
--   > $(rearrV [| f |]) b :: BiGUL s v
--
--   where @f :: v -> v'@ is a simple lambda-expression and @b :: BiGUL s v'@ an inner program.
--   In @f@, wildcard ‘@_@’ is not allowed, and all pattern variables must be used in the body.
--   (This is for ensuring that the view information is fully embedded into the source.)
rearrV :: Q TH.Exp  -- ^ rearranging lambda-expression
       -> Q TH.Exp
rearrV qlambexp = do
  lambexp <- qlambexp
  case lambexp of
    LamE [p] e ->
      let varnames = getAllVars e
          unusedVars = namesBoundInPat p \\ varnames
      in  if null unusedVars
          then rearr' RTag lambexp (varnames \\ nub varnames)
          else fail $ "Variable(s) unused in the body of a view-rearranging lambda-expression: " ++
                      concat (intersperse ", " (map nameBase unusedVars))
    LamE _   _ -> fail "A rearranging lambda-expression should have exactly one argument"
    _          ->
      fail "The first argument to rearrV should be a (quoted) lambda-expression"

mkExpFromPat :: TH.Pat -> Q TH.Exp
mkExpFromPat (LitP c) = return (LitE c)
mkExpFromPat (ConP name ps) = do
  es <- mapM mkExpFromPat ps
  return $ foldl (\acc e -> (AppE acc e)) (ConE name) es
mkExpFromPat (RecP name ps) = do
  rs <- mapM mkExpFromPat (map snd ps)
  let es = zip (map fst ps) rs
  return (RecConE name es)
mkExpFromPat (ListP ps) = do
  es <- mapM mkExpFromPat ps
  return (ListE es)
mkExpFromPat (InfixP pl name pr) = do
  epl <- mkExpFromPat pl
  epr <- mkExpFromPat pr
  return (InfixE (Just epl) (ConE name) (Just epr))
mkExpFromPat (TupP ps) = do
  es <- mapM mkExpFromPat ps
  return (TupE es)
mkExpFromPat (VarP name) = return (VarE name)
mkExpFromPat WildP = [| () |]
mkExpFromPat _ = fail "Unsupported pattern in a rearranging lambda-expression"

mkExpFromPat' :: TH.Pat -> Q TH.Exp
mkExpFromPat' (ConP name ps ) = do (_, [replace]) <- lookupNames astNamespace [] ["Replace"]
                                   ConP name' [] <- [p| () |]
                                   if name == name' && ps == []
                                   then return (ConE replace)
                                   else fail $ "Panic: rearrSV only supports tuple"
mkExpFromPat' (VarP name) = return (VarE name)
mkExpFromPat' (TupP ps) = do
  (_, [prod]) <- lookupNames astNamespace [] ["Prod"]
  es <- mapM mkExpFromPat' ps
  return $ foldr1 (\e1 e2 -> ((ConE prod `AppE` e1) `AppE` e2)) es
mkExpFromPat' _ = fail $ "Panic: rearrSV only supports tuple"

toProduct :: TH.Exp -> Q TH.Exp
toProduct (AppE e1 e2) = do
  (ConE unitn) <- [| () |]
  (_, [econst,ein,eleft,eright]) <- lookupNames astNamespace [] ["EConst", "EIn", "ELeft", "ERight"]
  re2 <- toProduct e2
  re1 <- toProduct e1
  if e1 == (ConE eleft) || e1 == (ConE eright) || e1 == (ConE ein)
  then return re2
  else if e1 == (ConE econst)
       then return (AppE e1 (ConE unitn))
       else return (AppE re1 re2)
toProduct other = return other

mkProdPatFromSHelper :: TH.Pat -> Q TH.Pat
mkProdPatFromSHelper (TupP []) = [p| () |]
mkProdPatFromSHelper other     = return other

-- | takes a source pattern and produces a tuple pattern consisting of all the variables in the source pattern
-- 1:s:ss -> (() , (s, ss))
mkProdPatFromS :: TH.Pat -> Q TH.Pat
mkProdPatFromS (LitP c) = [p| () |]
mkProdPatFromS (ConP name ps) = do
  es <- mapM mkProdPatFromS ps
  mkProdPatFromSHelper $ TupP es
mkProdPatFromS (RecP name ps) = do
  rs <- mapM mkProdPatFromS (map snd ps)
  mkProdPatFromSHelper (TupP rs)
mkProdPatFromS (ListP ps) = do
  es <- mapM mkProdPatFromS ps
  mkProdPatFromSHelper (TupP es)
mkProdPatFromS (InfixP pl name pr) = do
  epl <- mkProdPatFromS pl
  epr <- mkProdPatFromS pr
  return (TupP [epl,epr])
mkProdPatFromS (TupP ps) = do
  es <- mapM mkProdPatFromS ps
  mkProdPatFromSHelper (TupP es)
mkProdPatFromS (VarP name) = return (VarP name)
mkProdPatFromS WildP = [p| () |]
mkProdPatFromS _ = fail "Unsupported pattern in a rearranging lambda-expression"

-- | Example: rearrSV [p| x:xs |] [p| x:xs |] [p| (x,xs) |] [d| x = Replace; xs = rec |]
--   generates a rearrS from the first  pattern and the third pattern
--         and a rearrV from the second pattern and the third pattern
rearrSV :: Q TH.Pat -> Q TH.Pat -> Q TH.Pat -> Q [TH.Dec] -> Q TH.Exp
rearrSV qsp qvp qpp qpd = do
  (_, [edir,rearrs,rearrv]) <- lookupNames astNamespace [] ["EDir", "RearrS", "RearrV"]
  sp <- qsp
  vp <- qvp
  pp <- qpp
  pd <- qpd
  prodenv <-  mkEnvForUpdate pd
  let namesInPat = sort . map nameBase . namesBoundInPat
  checkVars (namesInPat sp) (namesInPat vp) (namesInPat pp) (Map.keys prodenv)
  spat <- mkPat sp STag []
  vpat <- mkPat vp RTag []
  commonexp <- mkExpFromPat pp
  commonexp' <- mkBodyExpForRearr commonexp
  commonexp'' <- toProduct commonexp'
  senv <- mkEnvForRearr sp
  sbody <- rearrangeExp commonexp'' (Map.map (ConE edir `AppE`) senv)
  venv <- mkEnvForRearr vp
  vbody <- rearrangeExp commonexp'' (Map.map (ConE edir `AppE`) venv)
  prodexp <- mkExpFromPat' pp
  prodbigul <- rearrangeExp prodexp prodenv
  return $ ((ConE rearrs `AppE` spat) `AppE` sbody) `AppE` (((ConE rearrv `AppE` vpat) `AppE` vbody) `AppE` prodbigul)
  where
    checkVars :: [String] -> [String] -> [String] -> [String] -> Q ()
    checkVars svars vvars cvars dvars | svars /= vvars =
      fail "Source and view patterns should have the same variables"
    checkVars svars vvars cvars dvars | svars /= cvars =
      fail "The common pattern should have the same variables as the source/view patterns"
    checkVars svars vvars cvars dvars | svars /= dvars =
      fail "The declaration list should include exactly the variables in the source/view patterns"
    checkVars svars vvars cvars dvars | otherwise      = return ()

-- | A succinct syntax dealing with the frequently occurring situation where both the source and view
--   are rearranged into products and their components further synchronised by inner updates.
--   For example, the program
--
--   > $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = b |]) :: BiGUL [a] [a]
--
--   matches both the source and view lists with a cons pattern, marking their head and tail as @x@ and @xs@ respectively,
--   and synchronises the heads using @Replace@ (which is the program associated with @x@ in the declaration list)
--   and the tails using some @b :: BiGUL [a] [a]@. In short, the program is equivalent to
--
--   > $(rearrS [| \(x:xs) -> (x, xs) |])$
--   >   $(rearrV [| \(x:xs) -> (x, xs) |])$
--   >     Replace `Prod` b
--
--   (Admittedly, it is an abuse of syntax to represent a list of named BiGUL programs in terms of a declaration list,
--   but it is the closest thing we can find that works directly with Template Haskell.)
update :: Q TH.Pat    -- ^ source pattern
       -> Q TH.Pat    -- ^ view pattern
       -> Q [TH.Dec]  -- ^ named updates (as a declaration list)
       -> Q TH.Exp
update ps pv d = rearrSV ps pv (ps >>= mkProdPatFromS) d

mkEnvForUpdate :: [TH.Dec] -> Q (Map String TH.Exp)
mkEnvForUpdate []                                     = return Map.empty
mkEnvForUpdate ((ValD (VarP name) (NormalB e) _ ):ds) = do
  renv <- mkEnvForUpdate ds
  return $ Map.singleton (nameBase name) e `Map.union` renv
mkEnvForUpdate (_:ds) = fail "Invalid syntax in update bindings (write ‘x1 = e1; x2 = e2; ...’)"

patCond :: TH.Pat -> Q TH.Exp
patCond p = do
  (_, [htrue]) <- lookupNames "Prelude" [] ["True"]
  return (LamE [p] (ConE htrue))

nameAdaptive :: Q TH.Exp
nameAdaptive = lookupNames astNamespace [] ["Adaptive"] >>= \(_, [badaptive]) -> conE badaptive

nameNormal :: Q TH.Exp
nameNormal = lookupNames astNamespace [] ["Normal"] >>= \(_, [bnormal]) -> conE bnormal

class ExpOrPat a where
  toExp :: a -> Q TH.Exp

instance ExpOrPat (Q TH.Exp) where
  toExp = id

instance ExpOrPat (Q TH.Pat) where
  toExp = (>>= patCond)

patLambdaToPred :: TH.Exp -> Q TH.Exp
patLambdaToPred p =
  case p of
    LamE [pat] body -> do
      (_, [hmaybe, hFalse, hid, hreturn]) <-lookupNames "Prelude" [] ["maybe", "False", "id", "return"]
      [| \x -> $(varE hmaybe) $(conE hFalse) $(varE hid) $(doExp hreturn pat [| x |] body) |]
    LamE [spat, vpat] body -> do
      (_, [hmaybe, hFalse, hid, hreturn]) <-lookupNames "Prelude" [] ["maybe", "False", "id", "return"]
      [| \s v -> $(varE hmaybe) $(conE hFalse) $(varE hid) $(doExp hreturn (TupP [spat, vpat]) [| (s, v) |] body) |]
    _ -> return p
  where
    doExp :: TH.Name -> TH.Pat -> Q TH.Exp -> TH.Exp -> Q TH.Exp
    doExp hreturn p qMatchExp boolExp = do
      matchExp <- qMatchExp
      return (DoE [BindS p (VarE hreturn `AppE` matchExp),
                   NoBindS (VarE hreturn `AppE` boolExp)])

-- | Construct a normal branch, for which a main condition on the source and view and
--   an exit condition on the source should be specified. The usual way of using 'normal' is
--
--   > $(normal [| p |] [| q |]) b :: CaseBranch s v
--
--   where
--
--   * @p :: s -> v -> Bool@,
--
--   * @q :: s -> Bool@, and
--
--   * @b :: BiGUL s v@, which is the branch body.
normal :: ExpOrPat a
       => Q TH.Exp  -- ^ main condition (binary predicate on the source and view)
       -> a         -- ^ exit condition (unary predicate on the source)
       -> Q TH.Exp
normal mp mq =
  [| \b -> ($(mp >>= patLambdaToPred), $(nameNormal) b $(toExp mq >>= patLambdaToPred)) |]

-- | A special case of 'normal' where the main condition is specified as the conjunction of two unary predicates
--   on the source and view respectively. The usual way of using 'normalSV' is
--
--   > $(normalSV [| ps |] [| pv |] [| q |]) b :: CaseBranch s v
--
--   where
--
--   * @ps :: s -> Bool@,
--
--   * @pv :: v -> Bool@,
--
--   * @q :: s -> Bool@, and
--
--   * @b :: BiGUL s v@, which is the branch body.
normalSV :: (ExpOrPat a, ExpOrPat b, ExpOrPat c)
         => a  -- ^ main source condition (unary predicate on the source)
         -> b  -- ^ main view condition (unary predicate on the view)
         -> c  -- ^ exit condition (unary predicate on the source)
         -> Q TH.Exp
normalSV mps mpv mq =
  [| \b -> (\s v -> $(toExp mps >>= patLambdaToPred) s && $(toExp mpv >>= patLambdaToPred) v,
            $(nameNormal) b $(toExp mq >>= patLambdaToPred)) |]

-- | Construct an adaptive branch, for which a main condition on the source and view should be specified.
--   The usual way of using 'adaptive' is
--
--   > $(adaptive [| p |]) f :: CaseBranch s v
--
--   where
--
--   * @p :: s -> v -> Bool@ and
--
--   * @f :: s -> v -> s@, which is the adaptation function.
adaptive :: Q TH.Exp  -- ^ main condition (binary predicate on the source and view)
         -> Q TH.Exp
adaptive mp = [| \f -> ($(mp >>= patLambdaToPred), $(nameAdaptive) f) |]

-- | A special case of 'adaptive' where the main condition is specified as the conjunction of two unary predicates
--   on the source and view respectively. The usual way of using 'adaptiveSV' is
--
--   > $(adaptiveSV [| ps |] [| pv |]) f :: CaseBranch s v
--
--   where
--
--   * @ps :: s -> Bool@,
--
--   * @pv :: v -> Bool@, and
--
--   * @f :: s -> v -> s@, which is the adaptation function.
adaptiveSV :: (ExpOrPat a, ExpOrPat b)
           => a  -- ^ main source condition (unary predicate on the source)
           -> b  -- ^ main view condition (unary predicate on the view)
           -> Q TH.Exp
adaptiveSV ps pv =
  [| \f -> (\s v -> $(toExp ps >>= patLambdaToPred) s && $(toExp pv >>= patLambdaToPred) v, $(nameAdaptive) f) |]

{-# LANGUAGE TemplateHaskell, TupleSections, TypeFamilies, ViewPatterns #-}

module CatLens where

import GHC.Generics
import Data.List
import Data.Function

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Generics.BiGUL.Interpreter


type Picture = String
type Name    = String

data FS = Directory Name [FS]
        | File Name Picture
        deriving (Show, Eq)

deriveBiGULGeneric ''FS

fsName :: FS -> Name
fsName (Directory name _) = name
fsName (File name _)      = name

isDirectory :: FS -> Bool
isDirectory (Directory _ _) = True
isDirectory (File _ _)      = False

type Tag = String
type Web = [(Picture, [Tag])]

pushdown :: BiGUL FS FS
pushdown = Checkpoint "pushdown" $ Case
  [ $(normalSV [p| Directory _ (Directory _ [] : _) |] [p| Directory _ _ |]
               [p| Directory _ (Directory _ [] : _) |])
    ==> $(update [p| Directory dirName (Directory _ [] : fs) |] [p| Directory dirName fs |]
                 [d| dirName = Replace
                     fs      = align (const True) (\s v -> fsName s == fsName v) Replace id (const Nothing) |])
  , $(normal [| \(Directory _ (Directory _ (s:_) : _)) (Directory _ (v:_)) -> fsName s == fsName v |]
             [p| Directory _ (Directory _ (_:_) : _) |])
    ==> $(rearrS [| \(Directory dirName (Directory dirName' (x:xs) : fs)) ->
                      (x, Directory dirName (Directory dirName' xs : fs)) |])$
          $(rearrV [| \(Directory dirName (x:xs)) -> (x, Directory dirName xs) |])$
            Replace `Prod` pushdown
  , $(adaptive [| \(Directory _ (Directory _ fs : _)) (Directory _ (v:_)) -> fsName v `elem` map fsName fs |])
    ==> \(Directory dirName (Directory dirName' fs : fs')) (Directory _ ((fsName -> name) : _)) ->
          Directory dirName (Directory dirName' (uncurry (:) (findFirst name fs)) : fs')
  , $(adaptive [| \(Directory _ (Directory _ fs' : fs)) (Directory _ (v:fs'')) ->
                    not (fsName v `elem` map fsName fs) && not (null (intersectBy ((==) `on` fsName) fs' fs'')) |])
    ==> \(Directory dirName (Directory dirName' fs' : fs)) (Directory _ (v:_)) ->
          Directory dirName (Directory dirName' (v:fs') : fs)
  , $(adaptiveSV [p| Directory _ (Directory _ (_:_) : _) |] [p| Directory _ _ |])
    ==> \(Directory dirName (Directory dirName' _ : fs)) _ -> Directory dirName (Directory dirName' [] : fs)
  ]
  where
    findFirst :: Name -> [FS] -> (FS, [FS])
    findFirst name (x:xs) | fsName x == name = (x, xs)
                          | otherwise        = let (y, ys) = findFirst name xs
                                               in  (y, x:ys)


catLensL :: BiGUL FS [Picture]
catLensL = Case
  [ $(normalSV [p| Directory _ [] |] [p| [] |] [p| Directory _ [] |])
    ==> $(update [p| _ |] [p| [] |] [d| |])
  , $(adaptiveSV [p| Directory _ (File _ _ : _) |] [p| [] |])
    ==> \(Directory dirName fs) _ -> Directory dirName (filter isDirectory fs)
  , $(normal [| \(Directory _ (File _ spic : _)) (vpic:_) -> spic == vpic |] [p| Directory _ (File _ _ : _) |])
    ==> $(rearrS [| \(Directory dirName (File _ pic : fs)) -> (pic, Directory dirName fs) |])$
          $(rearrV [| \(pic:pics) -> (pic, pics) |])$
            Replace `Prod` catLensL
  , $(normalSV [p| Directory _ (Directory _ _ : _) |] [p| _ |] [p| Directory _ (Directory _ _ : _) |])
    ==> pushdown `Compose` catLensL
  , $(adaptiveSV [p| Directory _ _ |] [p| _:_ |])
    ==> \dir (pic:_) -> let (file, Directory dirName fs) = maybe (File "???" pic, dir) id (findPic pic dir)
                        in  Directory dirName (file : fs)
  ]
  where
    findPic :: Picture -> FS -> Maybe (FS, FS)
    findPic pic (Directory _ []) = Nothing
    findPic pic (Directory dirName (file@(File _ pic') : fs))
      | pic == pic' = Just (file, Directory dirName fs)
      | otherwise   = do (file', Directory dirName' fs') <- findPic pic (Directory dirName fs)
                         return (file', Directory dirName' (file : fs'))
    findPic pic (Directory dirName (dir@(Directory _ _) : fs)) =
      case findPic pic dir of
        Just (file', dir') -> Just (file', Directory dirName (dir' : fs))
        Nothing -> do (file', Directory dirName' fs') <- findPic pic (Directory dirName fs)
                      return (file', Directory dirName' (dir : fs'))


catLensR :: BiGUL Web [Picture]
catLensR = align (const True) (\_ _ -> True) $(update [p| (pic, _) |] [p| pic |] [d| pic = Replace |]) (,[]) (const Nothing)

putR :: FS -> Web -> Maybe Web
putR fs web = put catLensR web =<< get catLensL fs

putL :: FS -> Web -> Maybe FS
putL fs web = put catLensL fs =<< get catLensR web


--------
-- Martin’s test cases

fs0 :: FS
fs0 = Directory "root"
        [ Directory "Jan"
            [ File "palindrome.jpg"
                   "cat0"
            , File "gamer.jpg"
                   "cat1" ]
        , Directory "May"
            [ File "froghead.jpg"
                   "cat2" ] ]

web0 :: Web
web0 = [ ("cat0", ["costume", "food"])
       , ("cat1", ["costume"        ])
       , ("cat2", ["costume"        ]) ]

fs1 :: FS
fs1 = Directory "root"
        [ Directory "Jan"
            [ File "palindrome.jpg"
                   "cat0"
            , File "gamer.jpg"
                   "cat1" ]
        , Directory "May"
            [ File "froghead.jpg"
                   "cat2" ]
        , File "???"
               "cat3" ]

web1 :: Web
web1 = [ ("cat0", ["costume", "food"])
       , ("cat1", ["costume"        ])
       , ("cat2", ["costume"        ])
       , ("cat3", ["onlyface"       ]) ]

-- adding to the web gallery
goal1 :: Bool
goal1 = putL fs0 web1 == Just fs1

fs2 :: FS
fs2 = Directory "root"
        [ Directory "Jan"
            [ File "palindrome.jpg"
                   "cat0"
            , File "gamer.jpg"
                   "cat1" ]
        , Directory "May"
            [ File "froghead.jpg"
                   "cat2" ]
        , File "burrito.jpg"
               "cat3" ]

web2 :: Web
web2 = web1

-- fixing the file name
goal2 :: Bool
goal2 = putR fs2 web1 == Just web2

fs3 :: FS
fs3 = fs2

web3 :: Web
web3 = [ ("cat0", ["costume", "food" ])
       , ("cat1", ["costume"         ])
       , ("cat2", ["costume"         ])
       , ("cat3", ["food", "onlyface"]) ]

-- changing tags
goal3 :: Bool
goal3 = putL fs2 web3 == Just fs3

fs4 :: FS
fs4 = Directory "root"
        [ Directory "Jan"
            [ File "palindrome.jpg"
                   "cat0"
            , File "gamer.jpg"
                   "cat1" ]
        , Directory "May"
            [ File "froghead.jpg"
                   "cat2" ]
        , File "burrito.jpg"
               "cat3"
        , File "whiteperson.jpg"
               "cat4" ]

web4 :: Web
web4 = [ ("cat0", ["costume", "food" ])
       , ("cat1", ["costume"         ])
       , ("cat2", ["costume"         ])
       , ("cat3", ["food", "onlyface"])
       , ("cat4", [                  ]) ]

-- adding to the file system
goal4 :: Bool
goal4 = putR fs4 web3 == Just web4

fs5 :: FS
fs5 = Directory "root"
        [ Directory "2010"
            [ Directory "Jan"
                [ File "palindrome.jpg"
                       "cat0"
                , File "gamer.jpg"
                       "cat1" ]
            , Directory "May"
                [ File "froghead.jpg"
                       "cat2" ] ]
        , Directory "2011"
            [ File "burrito.jpg"
                   "cat3"
            , File "whiteperson.jpg"
                   "cat4" ] ]

web5 :: Web
web5 = web4

-- restructuring
goal5 :: Bool
goal5 = putR fs5 web4 == Just web5

fs6 :: FS
fs6 = Directory "root"
        [ Directory "2010"
            [ Directory "Jan"
                [ File "gamer.jpg"
                       "cat1" ]
            , Directory "May"
                [ File "froghead.jpg"
                       "cat2" ] ]
        , Directory "2011"
            [ File "burrito.jpg"
                   "cat3"
            , File "whiteperson.jpg"
                   "cat4" ] ]

web6 :: Web
web6 = [ ("cat1", ["costume"         ])
       , ("cat2", ["costume"         ])
       , ("cat3", ["food", "onlyface"])
       , ("cat4", [                  ]) ]

-- delete from the web gallery
goal6 :: Bool
goal6 = putL fs5 web6 == Just fs6

fs7 :: FS
fs7 = Directory "root"
        [ Directory "2010"
            [ Directory "Jan"
                [ File "gamer.jpg"
                       "cat1" ]
            , Directory "May"
                [ File "froghead.jpg"
                       "cat2" ] ]
        , Directory "2011"
            [ File "burrito.jpg"
                   "cat3"
            , File "???"
                   "cat5"
            , File "whiteperson.jpg"
                   "cat4" ] ]

web7 :: Web
web7 = [ ("cat1", ["costume"         ])
       , ("cat2", ["costume"         ])
       , ("cat3", ["food", "onlyface"])
       , ("cat5", [                  ])
       , ("cat4", [                  ]) ]

-- insert to the middle of the web gallery
goal7 :: Bool
goal7 = putL fs6 web7 == Just fs7




goals :: [Bool]
goals = [goal1, goal2, goal3, goal4, goal5, goal6, goal7]

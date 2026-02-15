{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import GHC.Generics
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH

data List a = Nil | Con a (List a) deriving (Eq,Show)
deriveBiGULGeneric ''List

-- get-based thinking

ginner :: (Eq a, MonadError' ErrorInfo m) => BiGUL m (List a) (a, List a)
ginner = $(update [p| Con a b |] [d| a = Replace ; b = grotate |])

grotate :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
grotate = CaseS [$(normal [p| Nil|]) Replace,
                 $(normal [p| Con _ Nil |]) Replace,
                 $(normal [p| _ |]) (Compose  ($(rearr [| \(Con a (Con b c)) -> Con b (Con a c) |]) Replace)
                                              ($(rearr [| \(Con a b) -> (a,b) |]) ginner))
               ]

glistreverse :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
glistreverse = CaseS [ $(normal [p| Nil |]) Replace,
                       $(normal [p| Con _ _ |])  (Compose ($(rearr [| \(Con a b) -> (a,b) |])  $(update [p| Con a b |] [d| a = Replace ; b = glistreverse |])) grotate)
                     ]

-- put-based thinking

pinner :: (Eq a, MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
pinner = $(rearr [| \(Con a (Con b c)) -> Con b (Con a c) |]) Replace

protate :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
protate = CaseV [ $(branch [p| Nil|]) Replace,
                  $(branch [p| Con _ Nil |]) Replace,
                  $(branch [p| _ |]) $ CaseS [ $(adaptive [p| Nil |]) (\_ _ -> return (Con undefined Nil)) ,
                                               $(normal [p| Con _ _ |])
                                      ((flip Compose)  pinner
                                                ($(rearr [| \(Con a b) -> (a,b) |])
                                                 $(update [p| Con a b |] [d| a = Replace ; b = protate |]))) ]
                ]



plistreverse :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
plistreverse = CaseV [ $(branch [p| Nil |]) Replace,
                       $(branch [p| Con _ _ |]) $ CaseS [ $(adaptive [p| Nil |]) (\_ _ -> return (Con undefined Nil)) ,
                                                          $(normal [p| Con _ _ |])
                                                 ((flip Compose)
                                                       ($(rearr [| \(Con a b) -> (a,b) |])  $(update [p| Con a b |] [d| a = Replace ; b = plistreverse |]))
                                                       protate) ]
                     ]




{-
comparison between protate and grotate
In get direction:

*Main> testGet protate   (Con 1 Nil)
Right (Con 1 Nil)
*Main> testGet grotate   (Con 1 Nil)
Right (Con 1 Nil)
*Main> testGet protate   (Con 1 (Con 3 (Con 5 (Con 6 Nil))))
Right (Con 6 (Con 1 (Con 3 (Con 5 Nil))))
*Main> testGet grotate   (Con 1 (Con 3 (Con 5 (Con 6 Nil))))
Right (Con 3 (Con 5 (Con 6 (Con 1 Nil))))

They both work fine.

In put direction:

*Main> testPut protate   (Con 1 Nil)  (Con 1 (Con 2 (Con 3  (Con 5 Nil))))
Right (Con 2 (Con 3 (Con 5 (Con 1 Nil))))
*Main> testPut grotate   (Con 1 Nil)  (Con 1 (Con 2 (Con 3  (Con 5 Nil))))
Left (ErrorInfo "updated source does not satisfy the condition.")
*Main> testPut protate   (Con 1 (Con 3 (Con 5 Nil)))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 2 (Con 3 (Con 1 Nil)))
*Main> testPut grotate   (Con 1 (Con 3 (Con 5 Nil)))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 3 (Con 1 (Con 2 Nil)))
*Main> testPut protate   (Con 1 (Con 3 (Con 5 (Con 6 Nil))))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 2 (Con 3 (Con 1 Nil)))
*Main> testPut grotate   (Con 1 (Con 3 (Con 5 (Con 6 Nil))))  (Con 1 (Con 2 (Con 3  Nil)))
Left (ErrorInfo "updated source does not satisfy the condition.")

protate works fine.
But grotate works only if source and view have same length.

-}


{-
comparison between plistreverse and glistreverse
In get direction:

*Main> testGet plistreverse    (Con 1 Nil)
Right (Con 1 Nil)
*Main> testGet glistreverse    (Con 1 Nil)
Right (Con 1 Nil)
*Main> testGet plistreverse    (Con 1 (Con 3 (Con 5 (Con 6 Nil))))
Right (Con 6 (Con 5 (Con 3 (Con 1 Nil))))
*Main> testGet glistreverse    (Con 1 (Con 3 (Con 5 (Con 6 Nil))))
Right (Con 6 (Con 5 (Con 3 (Con 1 Nil))))

They both work fine.

In put direction:

*Main> testPut plistreverse    (Con 1 Nil)  (Con 1 (Con 2 (Con 3  (Con 5 Nil))))
Right (Con 5 (Con 3 (Con 2 (Con 1 Nil))))
*Main> testPut glistreverse    (Con 1 Nil)  (Con 1 (Con 2 (Con 3  (Con 5 Nil))))
Left (ErrorInfo "updated source does not satisfy the condition.")
*Main> testPut plistreverse    (Con 1 (Con 3 (Con 5 Nil)))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 3 (Con 2 (Con 1 Nil)))
*Main> testPut glistreverse    (Con 1 (Con 3 (Con 5 Nil)))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 3 (Con 2 (Con 1 Nil)))
*Main> testPut plistreverse    (Con 1 (Con 3 (Con 5 (Con 6 Nil))))  (Con 1 (Con 2 (Con 3  Nil)))
Right (Con 3 (Con 2 (Con 1 Nil)))
*Main> testPut glistreverse    (Con 1 (Con 3 (Con 5 (Con 6 Nil))))  (Con 1 (Con 2 (Con 3  Nil)))
Left (ErrorInfo "updated source does not satisfy the condition.")

plistreverse works fine.
But glistreverse works only if source and view have same length.
-}


{-
Notes:

The get-based approach is the simpler version of list_reverse in tree lens(treelens.hs),
it gives user a limited put behavior (valid when source and view have same length).

While put-based approach is much more flexible , it allows user to control the put behavior directly.

It's possible write put-based list_reverse using tree lens , but user have to go deep into each
tree lens' put semantics.

-}


-- put-based ==
plistreverse2 :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
plistreverse2 = CaseV [ $(branch [p| Nil |]) $ CaseS [ $(normal [p| Nil |]) Replace, $(normal [p| _ |]) Fail],
                        $(branch [p| Con _ _ |]) $ CaseS [ $(normal [p| Nil |]) Fail ,
                                                           $(normal [p| Con _ _ |])
                                                 ((flip Compose)
                                                       ($(rearr [| \(Con a b) -> (a,b) |])  $(update [p| Con a b |] [d| a = Replace ; b = plistreverse2 |]))
                                                       protate) ]
                     ]

-- put-based  >=
plistreverse3 :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
plistreverse3 = CaseV [ $(branch [p| Nil |]) Replace,
                        $(branch [p| Con _ _ |]) $ CaseS [ $(normal [p| Nil |]) Fail ,
                                                           $(normal [p| Con _ _ |])
                                                 ((flip Compose)
                                                       ($(rearr [| \(Con a b) -> (a,b) |])  $(update [p| Con a b |] [d| a = Replace ; b = plistreverse3 |]))
                                                       protate) ]
                     ]

-- put-based  <=
plistreverse4 :: (Eq a,MonadError' ErrorInfo m) => BiGUL m (List a) (List a)
plistreverse4 = CaseV [ $(branch [p| Nil |]) $ CaseS [ $(normal [p| Nil |]) Replace, $(normal [p| _ |]) Fail],
                        $(branch [p| Con _ _ |]) $ CaseS [ $(adaptive [p| Nil |]) (\_ _ -> return (Con undefined Nil))  ,
                                                           $(normal [p| Con _ _ |])
                                                 ((flip Compose)
                                                       ($(rearr [| \(Con a b) -> (a,b) |])  $(update [p| Con a b |] [d| a = Replace ; b = plistreverse4 |]))
                                                       protate) ]
                     ]
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns  #-}

import GHC.Generics
import Generics.BiGUL.Error
import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.List
import Data.Maybe
import Control.Monad.Except

---------------------------------------------------
--     Interpreter functions for BiGUL program
---------------------------------------------------
{-
put :: BiGUL s v -> s -> v -> Either (PutError s v) s
get :: BiGUL s v -> s -> Either (GetError s v) v

We provide two interpreter functions: put and get.
They take a BiGUL program and compute the derived function.
Noted that the result is wrapped in the Either monad.
-}

---------------------------------------------------
--             Replace :: BiGUL s s
---------------------------------------------------
{-
*Main> put Replace 1 2
Right 2

Replace requires that the source and view must have
the same type.
The put semantic is ignoring the source and returning
the view.
-}


---------------------------------------------------
--             Skip :: BiGUL s ()
---------------------------------------------------
{-
*Main> put Skip 1 ()
Right 1

Skip requires that the view must be the unit type.
The put semantic is remaining the source unchanged.
-}


---------------------------------------------------
--             Fail :: String -> BiGUL s v
---------------------------------------------------
{-
*Main> put (Fail "program fail") (1,2) 3
Left fail: program fail

Fail takes a string and returns a BiGUL program.
There are no requirements on source and view.
The put semantic is just printing the error message
passed in.
-}

---------------------------------------------------
--            Prod :: BiGUL s v -> BiGUL s' v'
--                 -> BiGUL (s, s') (v, v')
---------------------------------------------------
rplFst :: BiGUL (Int,Int) (Int,())
rplFst = Prod Replace Skip

{-
*Main> put rplFst (1,2) (3,())
Right (3,2)

Suppose the source has type (Int,Int) and the view has
type (Int,()). We want to update the fst part and remain
snd part unchanged.
We cannot write such a program just using three primitives
introduced before.
Here comes Prod, it uses two sub programs to synchronize
the fst and snd parts of the source and view tuple individually.
-}


---------------------------------------------------
--          RearrV :: Pat v env con -> Expr env v'
--                 -> BiGUL s v' -> BiGUL s v
---------------------------------------------------
rplFst' :: BiGUL (Int,Int) Int
rplFst' = $(rearrV [| \v -> (v,())|]) rplFst

{-
*Main> put rplFst' (1,2) 3
Right (3,2)

In the previous example, what we really want is that
view has type Int rather than (Int,()).
Here we can use RearrV, it first rearranges the view
to the form we want, and then uses a sub BiGUL program
to perform the transformation.

Noted that we write "$(rearrV [| \v -> (v,())|]) rplFst"
instead of "RearrV somePat someExpr rplFst".
"$(rearrV [| lambdaexpr |]) bigulProgram" is the syntax
sugar of RearrV, user just needs to specify the lambda
expression that rearranges the view and the sub BiGUL
program.

When rearranging the view, it's allowed to duplicate
information, but it's invalid to drop information.
"$(rearrV [| \v -> (v,v)|]) xxx" is valid.
"$(rearrV [| \(vl,vr) -> vl |]) xxx" is invalid.
-}

---------------------------------------------------
--         RearrS  :: Pat s env con -> Expr env s'
--                 -> BiGUL s' v -> BiGUL s v
---------------------------------------------------
{-
RearrS is similar to RearrV.
We also have syntax sugar
"$(rearrS [| lambdaexpr |]) bigulProgram"
for it.

It rearranges the source first and then uses a
sub BiGUL program to perform the transformation.

It's allowed to drop information when rearranging the source.
-}


---------------------------------------------------
--     Case :: [(s -> v -> Bool, CaseBranch s v)]
--             -> BiGUL s v
---------------------------------------------------
{-
Case [(psv1, Normal bigul1),
      (psv2, Normal bigul2),
      (psv3, Adaptive \s v -> s'),
      (psv4, Normal bigul3)
     ]

Case takes a list of tuples, the fst part is a predicate
on source and view, the snd part is either a normal branch
with a BiGUL program or a adaptive branch with a
s -> v -> s function.

When putting back, the predicates are tried one by one.
If the first matched branch is normal branch, the sub BiGUL
program is used ; otherwise a new source is computed by the
adaptive function, then we try from the first branch again,
if the matched branch is still a adaptive branch, it will
report error.

We provide a set of syntax sugars for Case.

Case [ $(normal [| ... |])     bigul1,
       $(normalS [| ... |])    bigul2,
       $(normalV [| ... |])    bigul3,
       $(normalSV [| ... |] [| ... |]) bigul4,
       $(adaptive [| ... |])   f1,
       ...
     ]

If the source and view have some relationship in the
predicate, user should use $(normal [| \s v -> ... |]) ;
If there is only predicate on source or view, user
should use $(normalS [| \s -> ... |]) or $(normalV [| \v -> ... |]) ;
Otherwise, the predicate is the conjunction of two predicates
on source and view, user should use
$(normalSV [| \s -> ... |] [| \v -> ... |]) ;

For normalS, normalV and normalSV, user can also use [p| somePattern |].
For example, the predicate is that source is non-empty list, user can
either write  $(normalS [| not . null |]) or  $(normalS [p| _:_ |]).

Correspondly,we have adaptive, adaptiveS, adaptiveV and adaptiveSV.
-}


---------------------------------------------------
--            More examples and sugar
---------------------------------------------------
iter :: Eq v => BiGUL s v -> BiGUL [s] v
iter b = Case
  [ $(normalS [p| [_] |])$
      $(rearrV [| \v -> (v,()) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` Skip
  , $(normalS [p| _:_ |])$
      $(rearrV [| \v -> (v, v) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` iter b
  ]

{-
We want to write a BiGUL program that synchronize every element of
the source list with a single view.

When the source list has one element, we synchronize this element
and remain the [] unchanged. So we first rearrange the view to
(view, ()) tuple, then rearrange the source list to tuple, finally we
synchronize these two tuples using BiGUl program (b `Prod` Skip).

When the source list has more than one element, it's similar to the
former case. We just need to copy the view and handle tail part using
the recursive call iter b.

Otherwise, the source list is empty, it will report error.
-}

iter' :: Eq v => BiGUL s v -> BiGUL [s] v
iter' b = Case
  [ $(normalS [p| [_] |])$
      $(update [p| v |] [p| v:_ |] [d| v = b |])
  , $(normalS [p| _:_ |])$
      $(rearrV [| \v -> (v, v) |])$
        $(rearrS [| \(s:ss) -> (s, ss) |])$
          b `Prod` iter' b
  ]

{-
We provide a new syntax sugar update for combination of rearrV and rearrS.

$(update [p| ... |] [p| ... |] [d| ... |]) has three parts:
view pattern, source pattern and declarations.

In this example, we can also write
$(update [p| v |] [p| v:_ |] [d| v = b |]) for the first case.
The view matchs the pattern variable v, the head element of the source
also matchs the variable v, then declaration v = b means we use the
program b to synchronize them. The _ in the source pattern means
that part is unchanged.

Noted that not every rearrV and rearrS combination can be written
this way. The second case is a counterexample.
-}

-- to be continued



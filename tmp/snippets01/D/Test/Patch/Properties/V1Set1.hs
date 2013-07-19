{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Test.Patch.Properties.V1Set1
       ( checkMerge, checkMergeEquiv, checkMergeSwap, checkCanon
       , checkCommute, checkCantCommute
       , tShowRead
       , tMergeEitherWayValid, tTestCheck ) where

import Darcs.Patch
     ( Patchy, commute, invert, merge, effect
     , readPatch, showPatch
     , fromPrim, canonize, sortCoalesceFL )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Merge ( Merge )
import qualified Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Test.Patch.Properties.Check ( checkAPatch, Check )
import Printer ( renderPS )
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Show
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
import Darcs.Witnesses.Unsafe( unsafeCoercePEnd )
import Darcs.Test.Util.TestResult
import Printer ( text )

#include "gadts.h"

type Patch = V1.Patch Prim


quickmerge :: (Patchy p, Merge p) => (p :\/: p ) C(x y) -> p C(y z)
quickmerge (p1:\/:p2) = case merge (p1:\/:p2) of
                        _ :/\: p1' -> unsafeCoercePEnd p1'

instance Show2 p => Show ((p :/\: p) C(x y)) where
   show (x :/\: y) = show2 x ++ " :/\\: " ++ show2 y
instance Show2 p => Show ((p :< p) C(x y)) where
   show (x :< y) = show2 x ++ " :< " ++ show2 y
instance MyEq p => Eq ((p :/\: p) C(x y)) where
   (x :/\: y) == (x' :/\: y') = isIsEq (x =\/= x') && isIsEq (y =\/= y')

-- ----------------------------------------------------------------------------
-- A number of "comparison" properties: these carry out some operation on
-- inputs (first value in the pair) and compare the results with a known
-- expected value (the second value in the pair).
--

checkMerge :: ((FL Patch:\/: FL Patch) C(x y), FL Patch C(y z)) -> TestResult
checkMerge (p1:\/:p2,p1') =
   case merge (p1:\/:p2) of
   _ :/\: p1a ->
       if isIsEq (p1a `eqFL` p1')
       then succeeded
       else failed $ text $ "Merge gave wrong value!\n"++show p1++show p2
            ++"I expected\n"++show p1'
            ++"but found instead\n"++show p1a

checkMergeEquiv :: ((FL Patch:\/:FL Patch) C(x y),FL Patch C(y z)) -> TestResult
checkMergeEquiv (p1:\/: p2, pe) =
    case quickmerge (p1:\/:p2) of
    p1' -> if checkAPatch (invert p1 :>: p2 :>: p1' :>: invert pe :>: NilFL)
           then succeeded
           else failed $ text $ "Oh no, merger isn't equivalent...\n"++show p1++"\n"++show p2
                 ++"in other words\n" ++ show (p1 :\/: p2)
                 ++"merges as\n" ++ show (merge $ p1 :\/: p2)
                 ++"merges to\n" ++ show (quickmerge $ p1 :\/: p2)
                 ++"which is equivalent to\n" ++ show (effect p1')
                 ++ "should all work out to\n"
                 ++ show pe

checkMergeSwap :: (FL Patch C(x y), FL Patch C(x z)) -> TestResult
checkMergeSwap (p1, p2) =
    case merge (p2:\/:p1) of
    _ :/\: p2' ->
        case merge (p1:\/:p2) of
        _ :/\: p1' ->
            case commute (p1:>p2') of
            Just (_:>p1'b) ->
                if not $ p1'b `eqFLUnsafe` p1'
                then failed $ text $ "Merge swapping problem with...\np1 "++
                      show p1++"merged with\np2 "++
                      show p2++"p1' is\np1' "++
                      show p1'++"p1'b is\np1'b  "++
                      show p1'b
                else succeeded
            Nothing -> failed $ text $ "Merge commuting problem with...\np1 "++
                        show p1++"merged with\np2 "++
                        show p2++"gives\np2' "++
                        show p2'++"which doesn't commute with p1.\n"

checkCanon :: FORALL(x y) (FL Patch C(x y), FL Patch C(x y)) -> TestResult
checkCanon (p1,p2) =
    if isIsEq $ eqFL p1_ p2
    then succeeded
    else failed $ text $ "Canonization failed:\n"++show p1++"canonized is\n"
          ++show (p1_ :: FL Patch C(x y))
          ++"which is not\n"++show p2
    where p1_ = mapFL_FL fromPrim $ concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect p1

checkCommute :: ((FL Patch:< FL Patch) C(x y), (FL Patch:< FL Patch) C(x y)) -> TestResult
checkCommute (p1:<p2,p2':<p1') =
   case commute (p2:>p1) of
   Just (p1a:>p2a) ->
       if (p2a:< p1a) == (p2':< p1')
       then succeeded
       else failed $ text $ "Commute gave wrong value!\n"++show p1++"\n"++show p2
             ++"should be\n"++show p2'++"\n"++show p1'
             ++"but is\n"++show p2a++"\n"++show p1a
   Nothing -> failed $ text $ "Commute failed!\n"++show p1++"\n"++show p2
   <&&>
   case commute (p1':>p2') of
   Just (p2a:>p1a) ->
       if (p1a:< p2a) == (p1:< p2)
       then succeeded
       else failed $ text $ "Commute gave wrong value!\n"++show p2a++"\n"++show p1a
             ++"should have been\n"++show p2'++"\n"++show p1'
   Nothing -> failed $ text $ "Commute failed!\n"++show p2'++"\n"++show p1'

checkCantCommute :: (FL Patch:< FL Patch) C(x y) -> TestResult
checkCantCommute (p1:<p2) =
    case commute (p2:>p1) of
    Nothing -> succeeded
    _ -> failed $ text $ show p1 ++ "\n\n" ++ show p2 ++
          "\nArgh, these guys shouldn't commute!\n"

-- ----------------------------------------------------------------------------
-- A few "test" properties, doing things with input patches and giving a OK/not
-- OK type of answer.

tShowRead :: (Show2 p, Patchy p) => (FORALL(x y w z) p C(x y) -> p C(w z) -> Bool) -> FORALL(x y) p C(x y) -> TestResult
tShowRead eq p =
    case readPatch $ renderPS $ showPatch p of
    Just (Sealed p') -> if p' `eq` p then succeeded
                        else failed $ text $ "Failed to read shown:  "++(show2 p)++"\n"
    Nothing -> failed $ text $ "Failed to read at all:  "++(show2 p)++"\n"

tMergeEitherWayValid :: FORALL(x y p) (Check p, Show2 p, Merge p, Patchy p) => (p :\/: p) C(x y) -> TestResult
tMergeEitherWayValid (p1 :\/: p2) =
  case p2 :>: quickmerge (p1:\/: p2) :>: NilFL of
  combo2 ->
    case p1 :>: quickmerge (p2:\/: p1) :>: NilFL of
    combo1 ->
      if not $ checkAPatch combo1
      then failed $ text $ "oh my combo1 invalid:\n"++show2 p1++"and...\n"++show2 p2++show combo1
      else
        if checkAPatch (invert combo1 :>: combo2 :>: NilFL)
        then succeeded
        else failed $ text $ "merge both ways invalid:\n"++show2 p1++"and...\n"++show2 p2++
              show combo1++
              show combo2

tTestCheck :: FORALL(x y) FL Patch C(x y) -> TestResult
tTestCheck p = if checkAPatch p
                 then succeeded
                 else failed $ text $ "Failed the check:  "++show p++"\n"

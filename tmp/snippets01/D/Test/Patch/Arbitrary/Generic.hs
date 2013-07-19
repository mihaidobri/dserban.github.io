{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}
{-# LANGUAGE CPP, UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses,
             FlexibleInstances, ViewPatterns #-}

#include "gadts.h"
module Darcs.Test.Patch.Arbitrary.Generic
       ( Tree(..), TreeWithFlattenPos(..), G2(..), ArbitraryPrim, NullPatch(..), RepoModel(..)
       , flattenOne, flattenTree, mapTree, sizeTree
       , commutePairFromTree, mergePairFromTree
       , commuteTripleFromTree, mergePairFromCommutePair
       , commutePairFromTWFP, mergePairFromTWFP, getPairs, getTriples
       , patchFromTree
       , canonizeTree
       , quickCheck
       ) where

import Control.Monad ( liftM )
import Test.QuickCheck
import Darcs.Test.Patch.WithState
import Darcs.Test.Patch.RepoModel
import Darcs.Test.Util.QuickCheck ( bSized )
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Unsafe
import Darcs.Witnesses.Ordered
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Invert(..), Commute(..) )
import Darcs.Patch.Prim ( PrimOf, PrimPatch, PrimPatchBase, FromPrim(..), PrimConstruct( anIdentity ) )
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.V2 ( RealPatch ) -- XXX this is more or less a hack
--import Darcs.ColorPrinter ( errorDoc )
--import Darcs.ColorPrinter ( traceDoc )
import Darcs.Witnesses.Show
--import Printer ( greenText, ($$) )

-- | Generate a patch to a certain state.
class ArbitraryStateIn s p where
  arbitraryStateIn :: s C(x) -> Gen (p C(x))

data Tree p C(x) where
   NilTree :: Tree p C(x)
   SeqTree :: p C(x y) -> Tree p C(y) -> Tree p C(x)
   ParTree :: Tree p C(x) -> Tree p C(x) -> Tree p C(x)

mapTree :: (FORALL(y z) p C(y z) -> q C(y z)) -> Tree p C(x) -> Tree q C(x)
mapTree _ NilTree = NilTree
mapTree f (SeqTree p t) = SeqTree (f p) (mapTree f t)
mapTree f (ParTree t1 t2) = ParTree (mapTree f t1) (mapTree f t2)

instance Show2 p => Show (Tree p C(x)) where
   showsPrec _ NilTree = showString "NilTree"
   showsPrec d (SeqTree a t) = showParen (d > appPrec) $ showString "SeqTree " .
                               showsPrec2 (appPrec + 1) a . showString " " .
                               showsPrec (appPrec + 1) t
   showsPrec d (ParTree t1 t2) = showParen (d > appPrec) $ showString "ParTree " .
                                 showsPrec (appPrec + 1) t1 . showString " " .
                                 showsPrec (appPrec + 1) t2

instance Show2 p => Show1 (Tree p) where
    showDict1 = ShowDictClass

instance Show2 p => Show1 (TreeWithFlattenPos p) where
    showDict1 = ShowDictClass

sizeTree :: Tree p C(x) -> Int
sizeTree NilTree = 0
sizeTree (SeqTree _ t) = 1 + sizeTree t
sizeTree (ParTree t1 t2) = 1 + sizeTree t1 + sizeTree t2

-- newtype G1 l p C(x) = G1 { _unG1 :: l (p C(x)) }
newtype G2 l p C(x y) = G2 { unG2 :: l (p C(x y)) }

flattenTree :: (Merge p) => Tree p C(z) -> Sealed (G2 [] (FL p) C(z))
flattenTree NilTree = seal $ G2 $ return NilFL
flattenTree (SeqTree p t) = mapSeal (G2 . map (p :>:) . unG2) $ flattenTree t
flattenTree (ParTree (flattenTree -> Sealed gpss1) (flattenTree -> Sealed gpss2))
                            = seal $ G2 $
                              do ps1 <- unG2 gpss1
                                 ps2 <- unG2 gpss2
                                 ps2' :/\: ps1' <- return $ merge (ps1 :\/: ps2)
                                 -- We can't prove that the existential type in the result
                                 -- of merge will be the same for each pair of
                                 -- ps1 and ps2.
                                 map unsafeCoerceP [ps1 +>+ ps2', ps2 +>+ ps1']

instance ArbitraryState s p => ArbitraryStateIn s (Tree p) where
  -- Don't generate trees deeper than 6 with default QuickCheck size (0..99).
  -- Note if we don't put a non-zero lower bound the first generated trees will always have depth 0.
  arbitraryStateIn rm = bSized 3 0.035 9 $ \depth -> arbitraryTree rm depth

-- | Generate a tree of patches, bounded by the depth @maxDepth@.
arbitraryTree :: ArbitraryState s p => s C(x) -> Int -> Gen (Tree p C(x))
arbitraryTree rm depth
    | depth == 0 = return NilTree
                      -- Note a probability of N for NilTree would imply ~(100*N)% of empty trees.
                      -- For the purpose of this module empty trees are useless, but even when
                      -- NilTree case is omitted there is still a small percentage of empty trees
                      -- due to the generation of null-patches (empty-hunks) and the use of canonizeTree.
    | otherwise  = frequency [(1, do Sealed (WithEndState p rm') <- arbitraryState rm
                                     t <- arbitraryTree rm' (depth - 1)
                                     return (SeqTree p t))
                             ,(3, do t1 <- arbitraryTree rm (depth - 1)
                                     t2 <- arbitraryTree rm (depth - 1)
                                     return (ParTree t1 t2))]


class NullPatch p where
  nullPatch :: p C(x y) -> EqCheck C(x y)

class (ArbitraryState (ModelOf prim) prim, NullPatch prim, PrimPatch prim, RepoModel (ModelOf prim)) => ArbitraryPrim prim

-- canonize a tree, removing any dead branches
canonizeTree :: NullPatch p => Tree p C(x) -> Tree p C(x)
canonizeTree NilTree = NilTree
canonizeTree (ParTree t1 t2)
    | NilTree <- canonizeTree t1 = canonizeTree t2
    | NilTree <- canonizeTree t2 = canonizeTree t1
    | otherwise = ParTree (canonizeTree t1) (canonizeTree t2)
canonizeTree (SeqTree p t) | IsEq <- nullPatch p = canonizeTree t
                           | otherwise = SeqTree p (canonizeTree t)


instance (RepoModel model, ArbitraryPrim prim, model ~ ModelOf prim,
          ArbitraryState model prim) => Arbitrary (Sealed (WithStartState model (Tree prim))) where
  arbitrary = do repo <- aSmallRepo
                 Sealed (WithStartState rm tree) <-
                     liftM (seal . WithStartState repo) (arbitraryStateIn repo)
                 return $ Sealed $ WithStartState rm (canonizeTree tree)

flattenOne :: (FromPrim p, Merge p) => Tree (PrimOf p) C(x) -> Sealed (FL p C(x))
flattenOne NilTree = seal NilFL
flattenOne (SeqTree p (flattenOne -> Sealed ps)) = seal (fromPrim p :>: ps)
flattenOne (ParTree (flattenOne -> Sealed ps1) (flattenOne -> Sealed ps2)) =
    --traceDoc (greenText "flattening two parallel series: ps1" $$ showPatch ps1 $$
    --          greenText "ps2" $$ showPatch ps2) $
    case merge (ps1 :\/: ps2) of
      ps2' :/\: _ -> seal (ps1 +>+ ps2')

data TreeWithFlattenPos p C(x) = TWFP Int (Tree p C(x))

commutePairFromTWFP :: (FromPrim p, Merge p, PrimPatchBase p)
                    => (FORALL (y z) (p :> p) C(y z) -> t)
                    -> (WithStartState model (TreeWithFlattenPos (PrimOf p)) C(x) -> t)
commutePairFromTWFP handlePair (WithStartState _ (TWFP n t))
    = unseal2 handlePair $
      let xs = unseal getPairs (flattenOne t)
      in if length xs > n && n >= 0 then xs!!n else seal2 (fromPrim anIdentity :> fromPrim anIdentity)

commutePairFromTree :: (FromPrim p, Merge p, PrimPatchBase p)
                    => (FORALL (y z) (p :> p) C(y z) -> t)
                    -> (WithStartState model (Tree (PrimOf p)) C(x) -> t)
commutePairFromTree handlePair (WithStartState _ t)
   = unseal2 handlePair $
     case flattenOne t of
       Sealed ps ->
         let xs = --traceDoc (greenText "I'm flattening one to get:" $$ showPatch ps) $
                 getPairs ps
         in if null xs then seal2 (fromPrim anIdentity :> fromPrim anIdentity) else last xs

commuteTripleFromTree :: (FromPrim p, Merge p, PrimPatchBase p)
                      => (FORALL (y z) (p :> p :> p) C(y z) -> t)
                      -> (WithStartState model (Tree (PrimOf p)) C(x) -> t)
commuteTripleFromTree handle (WithStartState _ t)
   = unseal2 handle $
     case flattenOne t of
       Sealed ps ->
         let xs = --traceDoc (greenText "I'm flattening one to get:" $$ showPatch ps) $
                  getTriples ps
         in if null xs
            then seal2 (fromPrim anIdentity :> fromPrim anIdentity :> fromPrim anIdentity)
            else last xs

mergePairFromCommutePair :: (Commute p, Invert p)
                         => (FORALL (y z) (p :\/: p) C(y z) -> t)
                         -> (FORALL (y z) (p :>   p) C(y z) -> t)
mergePairFromCommutePair handlePair (a :> b)
 = case commute (a :> b) of
     Just (b' :> _) -> handlePair (a :\/: b')
     Nothing -> handlePair (b :\/: b)

-- impredicativity problems mean we can't use (.) in the definitions below

mergePairFromTWFP :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
                  => (FORALL (y z) (p :\/: p) C(y z) -> t)
                  -> (WithStartState model (TreeWithFlattenPos (PrimOf p)) C(x) -> t)
mergePairFromTWFP x = commutePairFromTWFP (mergePairFromCommutePair x)

mergePairFromTree :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
                  => (FORALL (y z) (p :\/: p) C(y z) -> t)
                  -> (WithStartState model (Tree (PrimOf p)) C(x) -> t)
mergePairFromTree x = commutePairFromTree (mergePairFromCommutePair x)

patchFromCommutePair :: (Commute p, Invert p)
                     => (FORALL (y z) p C(y z) -> t)
                     -> (FORALL (y z) (p :> p) C(y z) -> t)
patchFromCommutePair handle (_ :> b) = handle b

patchFromTree :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
              => (FORALL (y z) p C(y z) -> t)
              -> (WithStartState model (Tree (PrimOf p)) C(x) -> t)
patchFromTree x = commutePairFromTree (patchFromCommutePair x)


instance Show2 p => Show (TreeWithFlattenPos p C(x)) where
   showsPrec d (TWFP n t) = showParen (d > appPrec) $ showString "TWFP " .
                            showsPrec (appPrec + 1) n . showString " " .
                            showsPrec1 (appPrec + 1) t

getPairs :: FL p C(x y) -> [Sealed2 (p :> p)]
getPairs NilFL = []
getPairs (_:>:NilFL) = []
getPairs (a:>:b:>:c) = seal2 (a:>b) : getPairs (b:>:c)

getTriples :: FL p C(x y) -> [Sealed2 (p :> p :> p)]
getTriples NilFL = []
getTriples (_:>:NilFL) = []
getTriples (_:>:_:>:NilFL) = []
getTriples (a:>:b:>:c:>:d) = seal2 (a:>b:>c) : getTriples (b:>:c:>:d)

instance (ArbitraryPrim prim, RepoModel (ModelOf prim), model ~ ModelOf prim,
          ArbitraryState model prim)
         => Arbitrary (Sealed (WithStartState model (TreeWithFlattenPos prim))) where
   arbitrary = do Sealed (WithStartState rm t) <- arbitrary
                  let num = unseal (length . getPairs) (flattenOneRP t)
                  if num == 0 then return $ Sealed $ WithStartState rm $ TWFP 0 NilTree
                    else do n <- choose (0, num - 1)
                            return $ Sealed $ WithStartState rm $ TWFP n t
                    where -- just used to get the length. In principle this should be independent of the patch type.
                          flattenOneRP :: Tree prim C(x) -> Sealed (FL (RealPatch prim) C(x))
                          flattenOneRP = flattenOne


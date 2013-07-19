{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}

#include "gadts.h"

module Darcs.Test.Patch.WithState
  where


import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Show
import Test.QuickCheck ( Gen, sized, choose )



----------------------------------------------------------------------
-- * WithState

data WithState s p C(x y) = WithState {
                              wsStartState :: s C(x)
                            , wsPatch      :: p C(x y)
                            , wsEndState   :: s C(y)
                            }
    deriving Eq

instance (Show1 s, Show2 p) => Show (WithState s p C(x y)) where
  showsPrec d (WithState s p s')
    = showParen (d > appPrec) $ showString "WithState "
                              . showsPrec1 (appPrec+1) s
                              . showString " "
                              . showsPrec2 (appPrec+1) p
                              . showString " "
                              . showsPrec1 (appPrec+1) s'

instance (Show1 s, Show2 p) => Show2 (WithState s p) where
  showDict2 = ShowDictClass

data WithStartState s p C(x) = WithStartState {
                                 wssStartState :: s C(x)
                               , wssPatch      :: p C(x)
                               }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithStartState s p C(x)) where
   showsPrec d (WithStartState s p) = showParen (d > appPrec) $ showString "WithStartState " .
                                      showsPrec1 (appPrec + 1) s . showString " " .
                                      showsPrec1 (appPrec + 1) p

instance (Show1 s, Show1 p) => Show1 (WithStartState s p) where
   showDict1 = ShowDictClass

-- | A combination of a patch and its final state. The state, in this module, is
--   typically represented by a 'RepoModel' value. The @px@ type is typically a
--   patch type applied to its pre-state, e.g. @Prim x@.
data WithEndState s px C(y) = WithEndState {
                                wesPatch    :: px C(y)
                              , wesEndState :: s C(y)
                              }
    deriving Eq

instance (Show1 s, Show1 p) => Show (WithEndState s p C(x)) where
   showsPrec d (WithEndState p s) = showParen (d > appPrec) $ showString "WithEndState " .
                                    showsPrec1 (appPrec + 1) p . showString " " .
                                    showsPrec1 (appPrec + 1) s


instance (Show1 s, Show1 p) => Show1 (WithEndState s p) where
   showDict1 = ShowDictClass


----------------------------------------------------------------------
-- * ArbitraryState generators

-- | A type class to generate arbitrary values, threading a state through the
--   arbitrary calls. So this can be used to generate a patch that comes after
--   another patch. The post-state of the generated patch is hidden by the
--   'Sealed'.
class ArbitraryState s p where
  arbitraryState :: s C(x) -> Gen (Sealed (WithEndState s (p C(x))))
  -- does a coarbitrary make sense?


instance ArbitraryState s p => ArbitraryState s (WithState s p) where
  arbitraryState s = do Sealed (WithEndState x s') <- arbitraryState s
                        return $ seal $ WithEndState (WithState s x s') s'


instance ArbitraryState s p => ArbitraryState s (p :> p) where
  arbitraryState s = do Sealed (WithEndState p1 s') <- arbitraryState s
                        Sealed (WithEndState p2 s'') <- arbitraryState s'
                        return $ seal $ WithEndState (p1 :> p2) s'' 

instance ArbitraryState s p => ArbitraryState s (p :> p :> p) where
  arbitraryState s0 = do Sealed (WithEndState p1 s1) <- arbitraryState s0
                         Sealed (WithEndState p2 s2) <- arbitraryState s1
                         Sealed (WithEndState p3 s3) <- arbitraryState s2
                         return $ seal $ WithEndState (p1 :> p2 :> p3) s3

arbitraryFL :: ArbitraryState s p => FORALL(x) Int -> s C(x) -> Gen (Sealed (WithEndState s (FL p C(x))))
arbitraryFL 0 s = return $ seal $ WithEndState NilFL s
arbitraryFL n s = do Sealed (WithEndState x s') <- arbitraryState s
                     Sealed (WithEndState xs s'') <- arbitraryFL (n-1) s'
                     return $ seal $ WithEndState (x :>: xs) s''

instance ArbitraryState s p => ArbitraryState s (FL p) where
  arbitraryState s = sized $ \n -> do k <- choose (0, min 2 (n `div` 5))
                                      arbitraryFL k s


makeS2Gen :: ArbitraryState s p => Gen (s C(x)) -> Gen (Sealed2 p)
makeS2Gen stGen = do s <- stGen
                     Sealed (WithEndState p _) <- arbitraryState s
                     return $ seal2 p

makeSGen :: ArbitraryState s p => Gen (s C(x)) -> Gen (Sealed (p C(x)))
makeSGen stGen = do s <- stGen
                    Sealed (WithEndState p _) <- arbitraryState s
                    return $ seal p

makeWS2Gen :: ArbitraryState s p => Gen (s C(x)) -> Gen (Sealed2 (WithState s p))
makeWS2Gen stGen = do s <- stGen
                      Sealed (WithEndState wsP _) <- arbitraryState s
                      return $ seal2 wsP

makeWSGen :: ArbitraryState s p => Gen (s C(x)) -> Gen (Sealed (WithState s p C(x)))
makeWSGen stGen = do s <- stGen
                     Sealed (WithEndState wsP _) <- arbitraryState s
                     return $ seal wsP

instance (Show2 p, Show1 s) => Show1 ((WithState s p) C(a)) where
  showDict1 = ShowDictClass


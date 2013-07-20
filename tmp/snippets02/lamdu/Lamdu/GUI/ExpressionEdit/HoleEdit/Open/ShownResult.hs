module Lamdu.GUI.ExpressionEdit.HoleEdit.Open.ShownResult
  ( ShownResult(..)
  , srPick
  ) where

import Data.Store.Guid (Guid)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Results as HoleResults
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data ShownResult m = ShownResult
  { srEventMap :: Widget.EventHandlers (T m)
  , srHoleResult :: Sugar.HoleResult Sugar.Name m HoleResults.SugarExprPl
  , srPickTo ::
    T m
    ( Maybe Guid -- Hole target guid
    , Widget.EventResult
    )
  }

srPick :: Functor m => ShownResult m -> T m Widget.EventResult
srPick = fmap snd . srPickTo

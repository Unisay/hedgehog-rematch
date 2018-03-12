module Hedgehog.Rematch
    ( verify
    ) where

import           Control.Rematch
import           Control.Rematch.Run
import           GHC.Stack
import qualified Hedgehog                   as H
import qualified Hedgehog.Internal.Property as I
import qualified Hedgehog.Internal.Source   as S

verify :: (H.MonadTest m, HasCallStack) => Matcher a -> a -> m ()
verify matcher a =
  case runMatch matcher a of
    MatchSuccess -> H.success
    MatchFailure msg -> I.liftTest $
      I.mkTest (Left $ I.Failure (S.getCaller callStack) msg Nothing, [])

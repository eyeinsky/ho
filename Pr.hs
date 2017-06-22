module Pr
  ( module Prelude
  , module Data.Monoid
  , module Data.String
  , module Data.Ratio
  , module Text.Read
  , module Data.Functor.Identity
  , module Control.Monad
  , module Control.Monad.Writer
  , module Control.Arrow
  , module GHC.Real
  ) where

import Prelude
import Data.Monoid
import Data.String
import Data.Ratio
import GHC.Real (Ratio(..))
import Text.Read hiding (lift)

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Writer
import Control.Arrow (first, second)

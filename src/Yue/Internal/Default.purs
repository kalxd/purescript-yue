module Yue.Internal.Default where

import Data.Maybe (Maybe(..))

-- | 默认值。
class Default a where
  def :: a

instance Default String where
  def = ""

instance Default Int where
  def = 0

instance Default (Array a) where
  def = []

instance Default (Maybe a) where
  def = Nothing

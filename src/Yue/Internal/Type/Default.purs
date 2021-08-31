module Yue.Internal.Type.Default where

import Data.Maybe (Maybe(..))

class Default a where
  def :: a

instance Default String where
  def = ""

instance Default Int where
  def = 0

instance Default Number where
  def = 0.0

instance Default (Array a) where
  def = []

instance Default a => Default (Maybe a) where
  def = Just def

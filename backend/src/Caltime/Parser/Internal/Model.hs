module Caltime.Parser.Internal.Model where

import Data.Time
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

data Arg
  = ArgAbs UTCTime
  | ArgDiff NominalDiffTime
  deriving (Generic, Show, Eq)

instance Arbitrary Arg where
  arbitrary = oneof [ArgAbs <$> arbitrary, ArgDiff <$> arbitrary]

data TimeOp = Add Arg Arg | Distract | Div
  deriving (Generic, Show, Eq)

data Error = ParseError Text
  deriving (Generic, Show, Eq)


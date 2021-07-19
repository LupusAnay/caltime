module Caltime.Parser.Internal.Model where

import Data.Time

data Arg = ArgAbs UTCTime | ArgDiff NominalDiffTime
  deriving (Generic, Show)

data TimeOp = Add Arg Arg | Distract | Div
  deriving (Generic, Show)

data Error = ParseError Text
  deriving (Generic, Show)


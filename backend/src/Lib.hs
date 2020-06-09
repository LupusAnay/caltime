module Lib (someFunc) where

import Data.Time
import Data.Attoparsec.Text as Atto

data Arg = ArgAbs UTCTime | ArgDiff NominalDiffTime
  deriving Generic

data TimeOp = Add Arg Arg | Distract | Div
  deriving Generic

data Error = ParseError Text
  deriving Generic

someFunc :: IO ()
someFunc = do
  let test_string = "2:37 + 4:24"

  traceM "fuck"

parseOps :: Text -> Either Error [TimeOp]
parseOps str = first (ParseError . toS) $ parseOnly p str
  where
    p = undefined


diffParser :: Parser NominalDiffTime
diffParser = Atto.takeWhile (not . isSpace) >>= parseTimeM True defaultTimeLocale "%H:%M:%S" . toS

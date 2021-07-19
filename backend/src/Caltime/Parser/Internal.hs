module Caltime.Parser.Internal where

import Data.Attoparsec.Text as Atto
import Data.Time
import Caltime.Parser.Internal.Model

diffParser :: Parser NominalDiffTime
diffParser = asum $ map timeParser formats
 where
  timeParser f = Atto.takeWhile (not . isSpace) >>= parseTimeM True defaultTimeLocale f . toS
  formats = ["%H:%M:%S", "%H:%M", "%H.%M.%S", "%H.%M"]

parseOps :: Text -> Either Error [TimeOp]
parseOps str = first (ParseError . toS) $ parseOnly p str
 where
  p = undefined

module Caltime.Parser.Internal where

import Data.Attoparsec.Text as Atto
import Data.Time
import Caltime.Parser.Internal.Model
import Control.Monad

isOp :: Char -> Bool
isOp t = t `elem` ['+', '-', '*']

diffParser :: Parser NominalDiffTime
diffParser = asum $ map timeParser formats
 where
  timeParser f = Atto.takeWhile (not . isSpace) >>= parseTimeM True defaultTimeLocale f . toS
  formats = ["%H:%M:%S", "%H:%M", "%H.%M.%S", "%H.%M"]

absParser :: Parser UTCTime
absParser = asum $ map timeParser formats
  where
    timeParser f = Atto.takeWhile (not . isSpace) >>= parseTimeM True defaultTimeLocale f . toS
    formats = ["%d-%m-%C%y", "%C%y-%m-%d", "%d.%m.%C%y", "%C%y.%m.%d"]

argParser :: Parser Arg
argParser = asum [ArgAbs <$> absParser, ArgDiff <$> diffParser]

opParser :: Parser TimeOp
opParser = do
  arg1 <- argParser <* Atto.skipSpace
  op <- operationParser <* Atto.skipSpace
  op arg1 <$> argParser <* Atto.skipSpace
  where
    operationParser :: Parser (Arg -> Arg -> TimeOp)
    operationParser = Atto.skipSpace *> Atto.takeWhile isOp >>= \case
      "+" -> pure Add
      x -> fail . toS $ "Operation " <> x <> " is not supported"

parseOps :: Text -> Either Error [TimeOp]
parseOps str = first (ParseError . toS) $ parseOnly p str
 where
  p = undefined

import Data.Time
import Test.Hspec
import Data.Attoparsec.Text
import Caltime.Parser.Internal

main :: IO ()
main = hspec $ do
  describe "Caltime.diffParser" $ do
    it "parse time in format %H:%M" $ do
      parseOnly diffParser "23:23" `shouldBe` (Right (84180 :: NominalDiffTime))

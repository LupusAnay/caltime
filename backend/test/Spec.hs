import Caltime.Parser.Internal
import Caltime.Parser.Internal.Model
import Data.Attoparsec.Text
import Data.Time
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Caltime.diffParser" $ do
    it "parse time in format %H:%M" $ do
      parseOnly diffParser "23:23" `shouldBe` Right (84180 :: NominalDiffTime)

    it "parse time in format %H:%M:%S" $ do
      parseOnly diffParser "23:23:23" `shouldBe` Right (84203 :: NominalDiffTime)

    it "parse time in format %H.%M.%S" $ do
      parseOnly diffParser "23.23.23" `shouldBe` Right (84203 :: NominalDiffTime)

    it "parse time in format %H.%M" $ do
      parseOnly diffParser "23.23" `shouldBe` Right (84180 :: NominalDiffTime)

    it "does not parse time in other formats" $ do
      parseOnly diffParser "someData" `shouldSatisfy` isLeft

  describe "Caltime.opParser" $
    it "parse single op in format %H:%M + %H:M" $ do
      parseOnly opParser "23:23 + 00:37" `shouldBe` Right (Add (ArgDiff 84180) (ArgDiff 2220))
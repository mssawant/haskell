import Test.Hspec
import Cipher

main :: IO ()
main = do
       hspec $ do
       describe "caesarCipherEncode" $ do
        it "caesarCipherEncode (Xyz, 3)  == Abc" $ do
            caesarCipherEncode "Xyz" 3 `shouldBe` "Abc"
       describe "caesarCipherDecode" $ do
        it "caesarCipherDecode (Abc, 3) == Xyz" $ do
            caesarCipherDecode "Abc" 3 `shouldBe "Xyz"
       describe "

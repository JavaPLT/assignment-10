import Test.Hspec

import Types
import Parser
import UnParser
import Simplifier

main :: IO ()
main = hspec $ do
    toIfTests
    headNormalizeTests
    normalizeTests
    evalTests
    toBoolTests
    reduceTests
    fileTests

toIfTests :: Spec
toIfTests =
  describe "Testing toIf" $
    it "on several expressions" $ do
    (toIf $ parseBoolExpr "(| (& x y) z)") `shouldBe` (parseIfExpr "(? (? x y F) T z)")
    (toIf $ parseBoolExpr "(> x (! y) )") `shouldBe` (parseIfExpr "(? x (? y F T) T)")

headNormalizeTests :: Spec
headNormalizeTests =
    describe "Testing headNormalize" $
        it "on several expressions" $ do
    (headNormalize (parseNExpr "x") (parseNExpr "y") (parseNExpr "z")) `shouldBe` parseNExpr "(? x y z)"
    (headNormalize (parseNExpr "T") (parseNExpr "y") (parseNExpr "z")) `shouldBe` parseNExpr "(? T y z)"
    (headNormalize (parseNExpr "F") (parseNExpr "y") (parseNExpr "z")) `shouldBe` parseNExpr "(? F y z)"
    (headNormalize (parseNExpr "(? x y z)") (parseNExpr "u") (parseNExpr "v")) `shouldBe` (parseNExpr "(? x (? y u v) (? z u v))")
    (headNormalize (parseNExpr "(? x (? yt yc ya) (? zt zc za))") (parseNExpr "u") (parseNExpr "v")) `shouldBe` (parseNExpr "(? x (? yt (? yc u v) (? ya u v )) (? zt (? zc u v) (? za u v)))")

normalizeTests :: Spec
normalizeTests =
    describe "Testing normalize" $
        it "on several expressions" $ do
    (normalize $ parseIfExpr "T") `shouldBe` parseNExpr "T"
    (normalize $ parseIfExpr "F") `shouldBe` parseNExpr "F"
    (normalize $ parseIfExpr "x") `shouldBe` parseNExpr "x"
    (normalize $ parseIfExpr "(? x y z)") `shouldBe` parseNExpr "(? x y z)"
    (normalize $ parseIfExpr "(? (? x y z) u v)") `shouldBe` parseNExpr "(? x (? y u v) (? z u v))"


evalTests :: Spec
evalTests =
    describe "Testing eval" $
        it "on several expressions" $ do
    pendingWith "add tests here"

toBoolTests :: Spec
toBoolTests =
    describe "Testing toBool" $
        it "on several expressions" $ do
    pendingWith "add tests here"

reduceTests :: Spec
reduceTests =
    describe "Testing reduce" $
        it "on several expressions" $ do
    (reduce $ parseBoolExpr "(| x (! x) )") `shouldBe` parseBoolExpr "T"
    (reduce $ parseBoolExpr "(& x (! x) )") `shouldBe` parseBoolExpr "F"
    (reduce $ parseBoolExpr "(& x T )") `shouldBe` parseBoolExpr "x"
    (reduce $ parseBoolExpr "(> (> (> p q) p) p)") `shouldBe` parseBoolExpr "T"


fileTest :: FilePath -> IO BoolExpr
fileTest f = reduce . parseBoolExpr <$> readFile f

fileTests :: Spec
fileTests =
    describe "Testing reduce" $ do
        it "on littleData1" $
            fileTest "data/littleData1" `shouldReturn` parseBoolExpr "T"
        it "on littleData2" $
            fileTest "data/littleData2" `shouldReturn` parseBoolExpr "T"
        it "on littleData3" $
            fileTest "data/littleData3"
                `shouldReturn`
                parseBoolExpr "(> h (> g (> f (> e (> d (> c (! b)))))))"
        it "on littleData4" $
            fileTest "data/littleData4"
                `shouldReturn`
                parseBoolExpr "(> h (> g (> f (> e (| d (| c (| b a)))))))"
        it "on bigData0" $
            fileTest "data/bigData0" `shouldReturn` parseBoolExpr "T"
        it "on bigData1" $
            fileTest "data/bigData1" `shouldReturn` parseBoolExpr "(> j (> i (> h (> g (> f (> e (| d (| c (| b a)))))))))"
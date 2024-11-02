import Chapter8
import Test.Hspec
import Text.Printf (vFmt)

main :: IO ()
main = hspec $ do
    describe "Chapter8 tests" $ do
        it "vars function" $ do
            let p1 = And (Var 'A') (Not (Var 'A'))
            let p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
            let p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
            let p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
            vars p1 `shouldBe` ['A', 'A']
            vars p2 `shouldBe` ['A', 'B', 'A']
            vars p3 `shouldBe` ['A', 'A', 'B']
            vars p4 `shouldBe` ['A', 'A', 'B', 'B']

        it "isTaut function" $ do
            let p1 = And (Var 'A') (Not (Var 'A'))
            let p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
            let p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
            let p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
            isTaut p1 `shouldBe` False
            isTaut p2 `shouldBe` True
            isTaut p3 `shouldBe` False
            isTaut p4 `shouldBe` True

        it "bools function" $ do
            let b = bools 4
            b `shouldBe` [[False, False, False, False],
                          [False, False, False, True],
                          [False, False, True,  False],
                          [False, False, True,  True],
                          [False, True,  False, False],
                          [False, True,  False, True],
                          [False, True,  True,  False],
                          [False, True,  True,  True],
                          [True,  False, False, False],
                          [True,  False, False, True],
                          [True,  False, True,  False],
                          [True,  False, True,  True],
                          [True,  True,  False, False],
                          [True,  True,  False, True],
                          [True,  True,  True,  False],
                          [True,  True,  True,  True]]

        it "substs function" $ do
            let p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
            let subs = substs p2
            subs `shouldBe` [[('A', False), ('B', False)],
                             [('A', False), ('B', True)],
                             [('A', True),  ('B', False)],
                             [('A', True),  ('B', True)]]

        it "value function" $ do
            let v = value (Add (Val 1) (Val 2))
            v `shouldBe` 3
        
        it "calculate on abstract machine" $ do
            let v = value' (Add (Add (Val 2) (Val 3)) (Val 4))
            v `shouldBe` 9

    describe "findTest function" $ do
        it "returns 'two' for input 2" $ do
            findTest 2 `shouldBe` "two"


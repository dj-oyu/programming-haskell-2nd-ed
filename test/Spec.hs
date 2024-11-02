import Chapter8
import Control.Exception (throw)

ch8tests :: Bool
ch8tests = do
    let p1 = And (Var 'A') (Not (Var 'A'))
    let p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
    let p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
    let p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

    let b = bools 4
    let subs = substs p2

    let v = value (Add (Val 1) (Val 2))

    and [
        vars p1 == ['A', 'A'],
        vars p2 == ['A', 'B', 'A'],
        vars p3 == ['A', 'A', 'B'],
        vars p4 == ['A', 'A', 'B', 'B'],
        isTaut p1 == False,
        isTaut p2 == True,
        isTaut p3 == False,
        isTaut p4 == True,
        b == [[False, False, False, False],
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
              [True,  True,  True,  True]],
        subs == [[('A', False), ('B', False)],
                 [('A', False), ('B', True)],
                 [('A', True),  ('B', False)],
                 [('A', True),  ('B', True)]],
        v == 3
        ]


main :: IO ()
main = do
        if ch8tests then putStrLn "Chapter8 tests passed" else throw (userError "Chapter8 tests failed")
        if findTest 2 == "two" then putStrLn "findTest passed" else throw (userError "findTest failed")
        putStrLn "All tests passed"


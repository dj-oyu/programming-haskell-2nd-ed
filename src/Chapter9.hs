module Chapter9 where

{- |
  == カウントダウン問題について
    @
    概要
      '100～999'の範囲の整数が与えられる。
      [1..10, 1..10, 25, 50, 75, 100]から任意の数を選んで
      四則演算のみを使って、与えられた整数を作る。
    @
  == 制約
    * 計算途中で負数にならない
    * 割り算は整数に割り切れるもののみ
-}
main :: IO ()
main =
    print "Let's play Countdown!" >> print (App Add (Val 1) (App Mul (Val 2) (Val 3)))

-- | 算術演算子
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- | 式のバリデーション
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- | 式の評価
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- | 数式
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
        where
            brak (Val n) = show n
            brak e       = "(" ++ show e ++ ")"

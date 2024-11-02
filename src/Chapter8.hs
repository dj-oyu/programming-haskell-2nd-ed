module Chapter8
    ( findTest
      , eval
      , vars
      , Prop(..)
      , Subst
      , bools
      , substs
      , isTaut
      , Expr(..)
      , value
    ) where

import Chapter7

-- | 連想リストの型宣言
type Assoc k v = [(k, v)]

{-|
  @
  指定されたキーに対応する値を連想リストから検索します。
  @
  
  == 引数
  * 'k' - 検索するキー
  * 't' - 連想リスト

  == 戻り値
  キーに対応する値
-}
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- | テスト用の連想リスト
testDict :: Assoc Int String
testDict = [(1, "one"), (2, "two"), (3, "three")]

{-|
  @
  テスト用の関数。指定されたキーに対応する文字列を返します。
  @
  
  == 引数
  * 'k' - 検索するキー
  
  == 戻り値
  キーに対応する文字列
-}
findTest :: Int -> String
findTest k = find k testDict

-- | 命題の構成要素
data Prop = Const Bool      -- ^ 真理値
          | Var Char        -- ^ 変数
          | Not Prop        -- ^ 否定
          | And Prop Prop   -- ^ 論理積
          | Imply Prop Prop -- ^ 含意(ならば)

-- | 変数の真理値リスト
type Subst = Assoc Char Bool

{-|
  @
  命題を評価します。
  @
  
  == 引数
  * 's' - 変数の真理値リスト
  * 'p' - 評価する命題
  
  == 戻り値
  命題の評価結果
-}
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

{-|
  @
  命題から変数を抽出します。
  @

  == 引数
  * 'p' - 抽出する命題
  
  == 戻り値
  命題に含まれる変数のリスト
-}
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

{-|
  @
  指定された桁数の真理値表を生成します。
  @
  
  == 引数
  * 'n' - 真理値表の桁数
  
  == 戻り値
  真理値表
-}
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n - 1)

{-|
  @
  命題に対する全ての真理値リストを生成します。
  @
  
  == 引数
  * 'p' - 命題
  
  == 戻り値
  真理値リストのリスト
-}
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

{-|
  @
  命題が恒真かどうかを判定します。
  @
  
  == 引数
  * 'p' - 命題
  
  == 戻り値
  命題が恒真であればTrue、そうでなければFalse
-}
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- | 整数と加算演算子からなる数式
data Expr = Val Int
          | Add Expr Expr

{-|
  @
  数式を評価します。
  @
  
  == 引数
  * 'e' - 評価する数式
  
  == 戻り値
  数式の評価結果
-}
value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y

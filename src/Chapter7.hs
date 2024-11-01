module Chapter7
    ( int2bin
    , rmdups
    )
    where

-- | ビット
type Bit = Int

{-|
  @
  整数を2進数のビット列に変換します。
  @
  
  == 引数
  * 'n' - 変換する整数
  
  == 戻り値
  2進数のビット列
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{-|
  @
  リストから重複する要素を削除します。
  @
  
  == 引数
  * 'xs' - 重複を削除するリスト
  
  == 戻り値
  重複が削除されたリスト
-}
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

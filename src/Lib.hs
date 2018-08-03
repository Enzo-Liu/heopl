module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  print $ maxSumOfList [1,-2,3,10,-4,7,2,-5]
  print $ maxProdOfList [-2.5,4,0,3,0.5,8,-1]

maxSumOfList :: [Int] -> (Int, Int, [Int])
maxSumOfList = flip foldr (minBound, 0, []) $
  \cur (allMax,lastMax, ls) ->
    let newlast = max cur (lastMax+cur)
        newmax = max allMax newlast in
      if newlast == cur
      then (newmax, newlast, [cur])
      else (newmax, newlast, ls++[cur])

maxProdOfList :: [Double] -> (Double, Double, Double)
maxProdOfList = flip foldr (0,1,1) $
  \cur (allMax,lastMax,lastMin) ->
    let choices = [cur,lastMax * cur,lastMin * cur]
        newLastMax = maximum choices
        newLastMin = minimum choices
        newMax = max allMax newLastMax in (newMax, newLastMax, newLastMin)

composable :: String -> String -> String -> Bool
composable "" "" "" = True
composable "" bs cs = bs == cs
composable as "" cs = as == cs
composable s1@(a:as) s2@(b:bs) (c:cs)
  | a == c && b == c = consumeA || consumeB
  | a == c = consumeA
  | b == c = consumeB
  | otherwise = False
  where consumeA = composable as s2 cs
        consumeB = composable s1 bs cs
composable _ _ _ = False

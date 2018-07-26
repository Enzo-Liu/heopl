module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  print $ maxSumOfList [1,-2,3,10,-4,7,2,-5]
  print $ maxProdOfList [-2.5,4,0,3,0.5,8,-1]

maxSumOfList :: [Int] -> (Int,Int)
maxSumOfList = flip foldr (minBound,0) $
  \cur (allMax,lastMax) ->
    let newlast = max cur (lastMax+cur)
        newmax = max allMax newlast in (newmax, newlast)

maxProdOfList :: [Double] -> (Double, Double, Double)
maxProdOfList = flip foldr (0,1,1) $
  \cur (allMax,lastMax,lastMin) ->
    let choices = [cur,lastMax * cur,lastMin * cur]
        newLastMax = maximum choices
        newLastMin = minimum choices
        newMax = max allMax newLastMax in (newMax, newLastMax, newLastMin)

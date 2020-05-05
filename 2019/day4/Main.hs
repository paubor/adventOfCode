import Data.List

hasDuplicateDigits :: Int -> Bool
hasDuplicateDigits n = length (show n) /= length (nub $ show n)

hasExactlyTwoDuplicateDigits :: Int -> Bool
hasExactlyTwoDuplicateDigits n = any (\g -> length g == 2) (group (show n))

increasingOrder :: Int -> Bool
increasingOrder n = show n == sort (show n)

inputRange = [231832..767346]
filtered = filter (\n -> increasingOrder n && hasExactlyTwoDuplicateDigits n) inputRange

main = do
  print (length filtered)
import Data.List

type Floor = Int
data Dimension = Dimension {
  l :: Int,
  w :: Int,
  h :: Int } deriving (Show)

newDimension :: Int -> Int -> Int -> Dimension
newDimension l w h = Dimension l w h


instance Read Dimension where
  readsPrec _ input =
    let l:w:h:rest = splitByX input
    in (\str -> [(newDimension
                  (read l ::Int)
                  (read w :: Int)
                  (read h :: Int), "")]) input where
      splitByX :: String -> [String]
      splitByX "" = []
      splitByX list = map (takeWhile (/='x') . tail) (filter (isPrefixOf ['x']) (tails ('x' : list )))

squareFeet :: Dimension -> Int
squareFeet (Dimension l w h) = 2*lw + 2*wh + 2*hl + minimum [lw, wh, hl] where
  lw = l * w
  wh = w * h
  hl = h * l

ribbon :: Dimension -> Int
ribbon (Dimension l w h) = (d0 + d0 + sorted !! 1 + sorted !! 1) + (d0 * sorted !! 1 * sorted !! 2) where
  [d0,d1,d2] = sort [l,w,h]

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let dimensions = map (\s -> read s :: Dimension) $ lines fileContents
  let sqf = sum $ map squareFeet $ dimensions
  let ribbons = sum $ map ribbon $ dimensions
  print ("SquareFeet  " ++ (show sqf))
  print ("Ribbon " ++ (show ribbons))
  return ()
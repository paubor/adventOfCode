type Floor = Int
data Direction = Up | Down deriving (Show)

toDirections :: String -> [Direction]
toDirections s = map toDirection s where
  toDirection :: Char -> Direction
  toDirection '(' = Up
  toDirection ')' = Down

findBasementPosition :: [Direction] -> (Floor, Maybe Int)
findBasementPosition directions = foldl move (0, Nothing) (zip [1..] directions) where
  move :: (Floor, Maybe Int) -> (Floor, Direction) -> (Floor, Maybe Int)
  move (floor, basementPosition) (i,direction) = (newFloor, newPosition) where
    newFloor = case direction of
            Up -> floor + 1
            Down -> floor -1
    newPosition = case (newFloor, basementPosition) of
            (-1, Nothing) -> Just i
            _ -> basementPosition

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let (floor, basementPos) = findBasementPosition $ toDirections fileContents
  print floor
  print (maybe "never!" show basementPos)
import qualified Data.List as L
import qualified Data.List.Split as S

data Layer = Layer [Color] deriving (Show)
data Color = Black | White | Transparent deriving (Show, Eq)

instance Ord Color where
  compare Black Black = EQ
  compare Black _ = GT
  compare _ Black = LT
  compare White Transparent = GT
  compare Transparent White = LT
  compare _ _ = EQ

-- instance Read Color where
readColor :: Char -> Color
readColor '0' = Black
readColor '1' = White
readColor '2' = Transparent

repl :: Char -> Char
repl '0' ='#'
repl '1' =' '

main = do
  fileContents <-  readFile "input.txt"
  let width = 25
  let height = 6
  -- let layers = map parseToLayer (S.chunksOf (width*height) fileContents)
  let flatPic = map (head . dropWhile (=='2')) $ L.transpose $ S.chunksOf (width*height) fileContents
  putStrLn (L.unlines $ S.chunksOf width $ flatPic)


data Direction = Up | Down | Left | Right
data Position = Position Int Int deriving (Show)

toDirection :: Char -> Direction
toDirection 'v' = Down
toDirection '^' = Up
toDirection '<' = Left
toDirection '>' = Right

toNewPosition :: Position -> Direction -> Position
toNewPosition (Position x y) Up = Position x (y+1)
toNewPosition (Position x y) Down = Position x (y-1)
toNewPosition (Position x y) Left = Position (x-1) y
toNewPosition (Position x y) Right = Position (x+1) y

visit :: [Position] -> Direction -> [Position]
visit positions direction = if (toNewPosition pos direction) `elem` positions then 
import Data.List.Split
-- import Data.Mabye

type Point = (Int, Int)
type Dir = Char
type Steps = Int

data Orientation = Hor | Vert
  deriving (Show, Eq)

data Move = M Dir Steps
  deriving Show



data Segment = S {
  start :: Point
  , move        :: Move
  , orientation :: Orientation
  , end         :: Point
} deriving Show

-- isHor :: Segment -> Bool
-- isHor segment = orientation s == Hd == 'L' || d== 'R'

-- isVert :: Segment -> Bool
-- isVert (S _ (M d _) _) = d == 'U' || d== 'D'


data Carrier = C {
  point :: Point
  ,  segments :: [Segment]
} deriving Show

defaultCarrier = C (0,0) []

readMove :: String -> Move
readMove (x:xs) = M x distance
  where distance = read xs


eval :: Carrier -> Move -> Carrier
eval (C (x,y) sgs) move@(M d steps) = C endPos ((S (x,y) move o endPos):sgs)
  where (endPos, o) =
          case d of
            'U' -> ((x, y+steps), Vert)
            'D' -> ((x, y-steps), Vert)
            'L' -> ((x-steps, y), Hor)
            'R' -> ((x+steps, y), Hor)


hor :: Carrier -> [Segment]
hor (C _ sgs) = filter (\s -> (orientation s) == Hor) sgs

vert :: Carrier -> [Segment]
vert (C _ sgs) = filter (\s -> (orientation s) == Vert) sgs


crosses :: [Segment] -> [Segment] -> [(Pos, Steps)]
crosses vs hs =
  let pairs = zip vs hs
  in catMaybes $ fmap cross pairs
  where
    cross :: Segment -> Segment -> Maybe Point
    cross v h = min 

moves1 = map readMove (splitOn "," "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
moves2 = map readMove (splitOn "," "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

carrier1 = foldl eval defaultCarrier moves1
carrier2 = foldl eval defaultCarrier moves2


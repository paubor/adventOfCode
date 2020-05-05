import Data.List.Split
import qualified Data.Set as S

type Orbit = (String,String)
data OrbitTree = OrbitTree String [OrbitTree]
  deriving (Show, Eq, Read)


createOrbitTree :: [Orbit] -> OrbitTree
createOrbitTree flatOrbits = createOrbitTree' (OrbitTree root []) flatOrbits
  where
    (parents, children) = unzip flatOrbits
    root = head . S.elems $ (S.fromList parents) `S.difference` (S.fromList children)
    createOrbitTree' (OrbitTree name _) flatOrbits' = OrbitTree name childs
      where
        childOrbits = filter (\(p,_) -> p == name) flatOrbits'
        childs = map (\(_, c) -> createOrbitTree' (OrbitTree c []) flatOrbits') childOrbits


parsePlanet :: String -> Orbit
parsePlanet s = (x,y) where
  (x,_:y) = break (==')') s

parse :: String -> OrbitTree
parse = createOrbitTree . map parsePlanet . lines

countOrbits :: OrbitTree -> Int
countOrbits = countOrbits' 0
  where countOrbits' depth (OrbitTree _ l) = depth + (sum $ map (countOrbits' (depth+1)) l)



distanceBetween :: String -> String -> OrbitTree -> Int
distanceBetween src dst o@(OrbitTree _ l)
  | not $ inOrbit src o || inOrbit dst o = 0
  | inOrbit src o && (not $ inOrbit dst o) = distance src o
  | inOrbit dst o && (not $ inOrbit src o) = distance dst o
  | otherwise = sum $ map (distanceBetween src dst) l
  where
    inOrbit :: String -> OrbitTree -> Bool
    inOrbit name (OrbitTree oName l) = name == oName || any (inOrbit name) l
    distance :: String -> OrbitTree -> Int
    distance = distance' 0
    distance' depth dst' o'@(OrbitTree _ l')
      | not $ inOrbit dst' o'= 0
      | any (\(OrbitTree n _) -> n == dst') l' = depth +1
      | otherwise = sum $ map (distance' (depth + 1) dst') l'



main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  let input = parse fileContents
  print $ countOrbits $ input
  print $ distanceBetween "YOU" "SAN" $ input
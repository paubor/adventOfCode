-- Domain
type Mass = Int
type Fuel = Int
type FuelCalculator = Mass -> Fuel

calculateFuel :: FuelCalculator
calculateFuel m = m `div` 3 - 2

calculateFuelAcc :: FuelCalculator
calculateFuelAcc m = sum $ drop 1 $ takeWhile (>= 0) $ iterate calculateFuel m

parseMasses :: [String] -> [Mass]
parseMasses stringlist = map (\s -> read s :: Mass) stringlist

fromFile :: FilePath -> FuelCalculator -> IO Fuel
fromFile fileName fuelCalculator = do
  fileContents   <- readFile fileName
  return (sum $ map fuelCalculator $ parseMasses $ lines fileContents)

fromFileSimple :: FilePath -> IO Fuel
fromFileSimple fileName = fromFile fileName calculateFuel

fromFileAcc :: FilePath -> IO Fuel
fromFileAcc fileName = fromFile fileName calculateFuelAcc

main :: IO ()
main = do
  simpleSum <- fromFileSimple "input.txt"
  accumulatedSum <- fromFileAcc "input.txt"
  putStrLn ("The total amount of fuel needed is " ++ show simpleSum)
  putStrLn ("The total amount of fuel needed taking into account fuel mass is " ++ show accumulatedSum)
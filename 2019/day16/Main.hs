import Data.Char

fixPhase:: Int -> [Int]
fixPhase n = drop 1 $ concatMap (replicate n) $ cycle [0,1,0,(-1)]


fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0 where
  addDigit num d = 10*num +d


fft :: [Int] -> [Int]
fft input = map getDigit $ zip [1..] input where
  getDigit :: (Int, Int) -> Int
  getDigit (position, value) = mod (abs (sumResult)) 10 where
    sumResult = sum $ zipWith (*) input (fixPhase position)

repeatFFT :: Int -> [Int] -> [Int]
repeatFFT n input = last $ take 101 $ iterate fft input

phase :: [Int] -> [Int]
phase input = phase' input (sum input) where
  phase' [] _ = []
  -- partialSum is the total amount for the current element
  phase' (x : xs) partialSum =
    -- in each iteration, one more input element ignored due to pattern (i.e. sum is less)
    (mod (abs partialSum) 10) : phase' xs (partialSum - x)

main = do
  fileInput <- readFile "input.txt"
  let input = map digitToInt fileInput
  let part1 = repeatFFT 100 input
  -- putStrLn "First part"
  -- let part1Result = concatMap (show) $ take 8 part1
  -- print part1Result
  putStrLn "Second part"
  let offset = read $ take 7 fileInput
  -- 10000 iterations means that we dont have to worry about the offset, so we can cut it off.
  let part2Input = drop offset $ concat $ replicate 10000 input
  let part2 = last $ take 101 $ iterate phase part2Input
  let part2Result = concatMap (show) $ take 8 part2
  print part2Result
  -- let part2 = repeatFFT 100 [0, 1, 0, -1] (concat $ replicate 10000 input)
  -- print $ take 8 $ drop messageOffset $ part2
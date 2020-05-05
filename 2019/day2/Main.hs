import Debug.Trace


run :: Int -> [Int] -> [Int]
run p program | code == 99 = program
         | code == 1 = run (p+4) (newList (+))
         | code == 2 = run (p+4) (newList (*))
         where
          [code, lidx, ridx, idx] = trace (show program) (sublist program p 4)
          sublist :: [a] -> Int -> Int -> [a]
          sublist list' position' offset' = drop position' . take (position'+offset') $ list'
          newList op = take idx program ++ op left right : drop (idx + 1) program
          [left, right] = [program !! lidx, program !! ridx]
          -- op = case code of
          --   1 -> (+)
          --   2 -> (*)
          -- xs'  = take idx xs ++ op (xs !! lidx) (xs !! ridx) : drop (idx + 1) xs

run2 :: Int -> [Int] -> Int
run2 ex xs = 100 * noun + verb
  where
    f x      = run 0 $ reset x 0 xs
    (x',_)   = head $ dropWhile (\(_,v) -> ex > v) [(x, f x) | x <- [0..99]]
    noun     = x' - 1
    g'       = reset noun
    f' x     = run 0 $ g' x xs
    (verb,_) = head $ dropWhile (\(_,v) -> ex /= v) [(x, f' x) | x <- [0..99]]

reset :: a -> a -> [a] -> [a]
reset x y xs = [head xs, x, y] ++ drop 3 xs


main :: IO ()
main = do
  putStr "Part 01: "
  fileContents <-  readFile "input.txt"
  let program = read (('[':fileContents)++[']']) :: [Int]
  let preppedProgram = reset 12 2 program
  print $  run 0 (reset 12 2 program)
  -- putStr "Part 02: "
  -- print $ run2 19690720 program
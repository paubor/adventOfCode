import Debug.Trace
import Data.List.Split
import Data.List(permutations)
-- import Data.Maybe
import Data.Char


replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt _ _ [] = []
replaceAt n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:(replaceAt (n-1) newVal xs)

readInt :: String -> Int
readInt = read

digits :: Int -> [Int]
digits = map digitToInt . show

type Pos = Int

data Param = Position Int | Immediate Int

instance Show Param where
  show (Immediate a) = show a
  show (Position a) = "@"<>(show a)

data Command = Op { id:: String, op :: (Int -> Int -> Int), a :: Param, b :: Param, result :: Pos, ip :: Pos } 
  | I  { id:: String, result :: Pos, ip :: Pos }
  | O  { id:: String, value :: Param, ip :: Pos }
  | J  { id:: String, cond :: (Int -> Int -> Bool), a :: Param, b :: Param, c :: Param, ip :: Pos }
  | C  { id:: String, cond :: (Int -> Int -> Bool), a :: Param, b :: Param, result :: Pos, ip :: Pos }
  | Halt

instance Show Command where
  show (Op id _ a b result ip) = show a<>" "<>id<>" "<>show b<>" = "<>show result<>", next: "<>show ip
  show (O _ value ip) = "write "<>show value<>", next: "<>show ip
  show (I _ value ip) = "read "<>show value<>", next: "<>show ip
  show (J id _ a _ result ip) = "jump "<>show result<>" if "<>show a<>" "<>id<>" 0 else "<>show ip
  show (C id _ a b result ip) = show result<>"= 1"<>" if "<>show a<>" "<>id<>" "<>show b<>" else 0"<>", next: "<>show ip
  show Halt = "stop"

getParam :: Int -> [Int] -> Pos -> Param
getParam val []  _     = Position val
getParam val (1:xs) 0 = Immediate val
getParam val (0:xs) 0 = Position val
getParam val (_:xs) n = getParam val xs (n-1)

splitOpAndP :: Int -> (Int,Int)
splitOpAndP n = n `divMod` 100

parse :: [Int] -> Pos -> Command
parse program n =
  let operation:params = drop n program in
  let (paramMode,op) = splitOpAndP operation in
  let reverseParamModes = reverse $ digits paramMode in
  let getModeParam idx = getParam (params !! idx) reverseParamModes idx in
  let getRawParam idx = params !! idx in
  case op of
    1 -> Op "+"   (+)   (getModeParam 0) (getModeParam 1) (getRawParam 2) (n+4)
    2 -> Op "*"   (*)   (getModeParam 0) (getModeParam 1) (getRawParam 2) (n+4)
    3 -> I  "in"        (getRawParam 0)                                   (n+2)
    4 -> O  "out"       (getModeParam 0)                                  (n+2)
    5 -> J  "/=" (/=)  (getModeParam 0) (Immediate 0)    (getModeParam 1) (n+3)
    6 -> J  "="  (==)  (getModeParam 0) (Immediate 0)    (getModeParam 1) (n+3)
    7 -> C  "LT"  (<)   (getModeParam 0) (getModeParam 1) (getRawParam 2) (n+4)
    8 -> C  "EQ"  (==)  (getModeParam 0) (getModeParam 1) (getRawParam 2) (n+4)
    99 -> Halt
    c -> error ("not recognized opCode at position "++show n++": "++show c)


type InOut = ([Int],[Int])

data Context = Ctx { in_out ::InOut, program :: [Int], nextPos :: Maybe Pos}
  deriving Show

evalParam :: Param -> [Int] -> Int
evalParam (Immediate v) _   = v
evalParam (Position addr) prog = prog !! addr

evalOp :: (Int -> Int -> Int) -> Param -> Param -> [Int] -> Int
evalOp op a b prog =
  let aVal = evalParam a prog in
  let bVal = evalParam b prog in
  op aVal bVal

evalCond :: (Int -> Int -> Bool) -> Param -> Param -> [Int] -> Bool
evalCond cond a b prog = cond (evalParam a prog) (evalParam b prog)

eval :: Command -> Context -> Context
eval Halt                     (Ctx io prog _)      = Ctx io prog Nothing-- can be general pattern
eval (Op _ op a b result ip)  (Ctx io prog _)      = Ctx io (replaceAt result (evalOp op a b prog) prog) (Just ip)
eval (I _ result ip)          (Ctx (i:is,o) prog _) = Ctx (is,o) (replaceAt result i prog) (Just ip)
eval (I _ result ip)          _                     = error "Got input instruction without input"
eval (O _ value ip)           (Ctx (i,o) prog _)      = Ctx (i,(evalParam value prog):o) prog (Just ip)
eval (J _ cond a b result ip) (Ctx io prog _)      = Ctx io prog (Just newIp) where
                                                        newIp = if evalCond cond a b prog then (evalParam result prog) else ip
eval (C _ cond a b result ip) (Ctx io prog _)      = Ctx io (replaceAt result value prog) (Just ip) where
                                                        value = if evalCond cond a b prog then 1 else 0

run :: Maybe Pos -> Context -> Context
run Nothing c = c
run (Just n) c@(Ctx io currentProgram next) =
  let command = parse currentProgram n in
  let nextCtx = eval command c in
  let nextIP = nextPos nextCtx in
    run nextIP nextCtx

run2 :: Int -> [Int] -> [Int] -> [Int]
run2 i program inputs =
  case opCode `mod` 100 of
    1 -> run2 (i+4) (replaceAt (addr 3) (arg 1 + arg 2) program) inputs
    2 -> run2 (i+4) (replaceAt (addr 3) (arg 1 * arg 2) program ) inputs
    3 -> run2 (i+2) (replaceAt (addr 1) (head inputs) program) (tail inputs)
    4 -> arg 1: run2 (i+2) program inputs
    5 -> run2 (if arg 1 == 0 then i+3 else arg 2) program inputs
    6 -> run2 (if arg 1 == 0 then arg 2 else i+3) program inputs
    7 -> run2 (i+4) (replaceAt (addr 3) (if arg 1 < arg 2 then 1 else 0) program) inputs
    8 -> run2 (i+4) (replaceAt (addr 3) (if arg 1 == arg 2 then 1 else 0) program) inputs
    99 -> []
    _ -> error "unknown opcode"
  where
    opCode = program !! i
    arg :: Int -> Int
    arg n
      | opCode `mod` (100*10^n) <= 10*10^n = program !! (addr n)
      | otherwise = addr n
    addr n = program !! (i + n)

runAmplifiers :: [Int] -> [Int] -> [Int]
runAmplifiers phase program = foldr (\p -> \out -> run2 0 program (p:out)) [0] phase

runAmplifiersLoop :: [Int] -> [Int] -> Int
runAmplifiersLoop phase program = last output
  where
    output = foldr runProgram (0: output) phase
    runProgram ph out = run2 0 program $ (ph:out)
-- runAmplifiersLoop phase program = last output
--   where output = foldr r (0: output) phase
--         r p o = traceShow (p) runProgram program $ (p:o)

main :: IO ()
main = do
  fileContents <-  readFile "input.txt"
  let program = map readInt $ splitOn "," "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"--fileContents
  -- let (maxThrusterSignal,phase) = maximum [(runAmplifiers p program,p) | p <- permutations [0..4]]
  -- print (359142 == maxThrusterSignal)
  -- let (maxThrusterWithLoop,phase) = maximum [(runAmplifiersLoop p program,p) | p <- permutations [5..9]]
  print (runAmplifiersLoop [9,8,7,6,5] program)


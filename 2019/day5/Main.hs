import Data.List.Split
import Data.Maybe
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



data Context = Ctx { input :: [Int], output :: [Int], program :: [Int], nextPos :: Maybe Pos}
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
eval Halt                     (Ctx i o prog _)      = Ctx i o prog Nothing-- can be general pattern
eval (Op _ op a b result ip)  (Ctx i o prog _)      = Ctx i o (replaceAt result (evalOp op a b prog) prog) (Just ip)
eval (I _ result ip)          (Ctx (i:is) o prog _) = Ctx is o (replaceAt result i prog) (Just ip)
eval (I _ result ip)          (Ctx i o prog _)      = error "Got input instruction without input"
eval (O _ value ip)           (Ctx i o prog _)      = Ctx i ((evalParam value prog):o) prog (Just ip)
eval (J _ cond a b result ip) (Ctx i o prog _)      = Ctx i o prog (Just newIp) where
                                                        newIp = if evalCond cond a b prog then (evalParam result prog) else ip
eval (C _ cond a b result ip) (Ctx i o prog _)      = Ctx i o (replaceAt result value prog) (Just ip) where
                                                        value = if evalCond cond a b prog then 1 else 0

run :: Maybe Pos -> Context -> Context
run Nothing c = c
run (Just n) c@(Ctx i o currentProgram next) =
  let command = parse currentProgram n in
  let nextCtx = eval command c in
  let nextIP = nextPos nextCtx in
    run nextIP nextCtx

runProgram :: Pos -> [Int] -> [Int] -> Context
runProgram p i prog = run (Just p) (Ctx i [] prog Nothing)

main :: IO ()
main = do
  fileContents <-  readFile "input.txt"
  -- let fileContents = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
  let program = map readInt $ splitOn "," fileContents
  let c = runProgram 0 [5] program
  print (output c)


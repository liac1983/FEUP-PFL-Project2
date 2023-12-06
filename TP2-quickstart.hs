-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List (intercalate)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


-- Add the following definition for Stack
type Stack = [Integer]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

-- stack2Str :: Stack -> String
stack2Str :: Stack -> String
stack2Str stack = unwords $ map show stack

-- Add the following definition for State
type State = [(String, Integer)]

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []

-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- state]

-- run :: (Code, Stack, State) -> (Code, Stack, State)
-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- If the code is empty, return the same state
run (inst:code, stack, state) = case inst of
  -- Implement the logic for each instruction
  Push n -> run (code, n:stack, state)
  Add -> run (code, performBinaryOperation (+) stack, state)
  Mult -> run (code, performBinaryOperation (*) stack, state)
  Sub -> run (code, performBinaryOperation (\x y -> y - x) stack, state)  -- Fix: Correct order of subtraction
  Tru -> run (code, 1:stack, state)
  Fals -> run (code, 0:stack, state)
  Equ -> run (code, performBinaryOperation (\x y -> if x == y then 1 else 0) stack, state)
  Le -> run (code, performBinaryOperation (\x y -> if x <= y then 1 else 0) stack, state)
  And -> run (code, performBinaryOperation (\x y -> if x /= 0 && y /= 0 then 1 else 0) stack, state)
  Neg -> run (code, negate (head stack):tail stack, state)
  Fetch x -> run (code, lookupVar x state:stack, state)
  Store x -> run (code, tail stack, updateVar x (stack !! 0) state)  -- Fix: Use value from the stack
  Noop -> run (code, stack, state)
  Branch c1 c2 -> if head stack /= 0 then run (c1 ++ code, tail stack, state)
                  else run (c2 ++ code, tail stack, state)
  Loop c1 c2 -> if head stack /= 0 then run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, tail stack, state)
                else run (code, tail stack, state)

-- Helper function to perform binary operations on the stack
performBinaryOperation :: (Integer -> Integer -> Integer) -> Stack -> Stack
performBinaryOperation op (x:y:rest) = op y x : rest

-- Helper function to lookup variable in the state
lookupVar :: String -> State -> Integer
lookupVar x state = case lookup x state of
  Just val -> val
  Nothing -> error ("Variable not found: " ++ x)

-- Helper function to update variable in the state
updateVar :: String -> Integer -> State -> State
updateVar x val state = filter (\(var, _) -> var /= x) state ++ [(x, val)]

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","") Passou 
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
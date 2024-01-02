-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List (intercalate)
import Data.Char (isSpace, isAlphaNum)
import Data.List.Split (chunksOf, splitOn)

-- Part 1

-- Do not modify our definition of Inst and Code
-- data Inst =
  -- Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  -- Branch Code Code | Loop Code Code
  -- deriving Show
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
stack2Str stack = intercalate "," (map showValue stack)

showValue :: Integer -> String
showValue x
  | x == 0    = "False"
  | x == 1    = "True"
  | otherwise = show x

-- Add the following definition for State
type State = [(String, Integer)]

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []

-- Atualização da função state2Str
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- reverse state]


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
  Fetch x -> run (code, lookupVarBool x state:stack, state)
  Store x -> run (code, tail stack, store x state stack)  -- Fix: Use value from the stack
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

-- Adicione a função lookupVarBool
lookupVarBool :: String -> State -> Integer
lookupVarBool x state = case lookup x state of
  Just val -> if val /= 0 then 1 else 0
  Nothing -> error ("Variable not found: " ++ x)

-- Helper function to update variable in the state
updateVar :: String -> Integer -> State -> State
updateVar x val state =
  case lookup var state of
    Just _  -> filter (\(v, _) -> v /= var) state ++ [(var, val)]
    Nothing -> (var, val) : state

-- Helper function to store variable in the state
store :: String -> State -> Stack -> State
store var state stack = updateVar var (head stack) state

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

-- Define the types Aexp, Bexp, and Stm

-- Arithmetic expressions
data Aexp
  = Num Integer         -- Numeric constant
  | Var String          -- Variable
  | AddA Aexp Aexp      -- Addition
  | SubA Aexp Aexp      -- Subtraction
  | MultA Aexp Aexp     -- Multiplication
  deriving Show

-- Boolean expressions
data Bexp
  = TrueExp             -- Boolean true
  | FalseExp            -- Boolean false
  | Not Bexp            -- Logical negation
  | EqA Aexp Aexp       -- Equality
  | LeA Aexp Aexp       -- Less than or equal to
  | AndB Bexp Bexp      -- Logical conjunction
  deriving Show

-- Statements
data Stm
  = Assign String Aexp  -- Assignment
  | Seq Stm Stm         -- Sequence of statements
  | If Bexp Stm Stm     -- Conditional statement
  | While Bexp Stm      -- While loop
  deriving Show



-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddA a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubA a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MultA a1 a2) = compA a1 ++ compA a2 ++ [Mult]

-- compB :: Bexp -> Code
compB :: Bexp -> Code
compB TrueExp = [Tru]
compB FalseExp = [Fals]
compB (Not b) = compB b ++ [Neg]
compB (EqA a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (LeA a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndB b1 b2) = compB b1 ++ compB b2 ++ [And]

-- compile :: Program -> Code
compile :: [Stm] -> Code
compile [] = []
compile (s:stmts) = case s of
  Assign var aexp -> compA aexp ++ [Store var] ++ compile stmts
  Seq stm1 stm2 -> compile [stm1, stm2] ++ compile stmts
  If bexp stm1 stm2 -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile stmts
  While bexp stm1 -> compile [stm1, While bexp stm1] ++ compB bexp ++ [Branch (compile [stm1, While bexp stm1]) [Noop]] ++ compile stmts
{-
-- Define the lexer function
lexer :: String -> [String]
lexer [] = []
lexer input = case span isTokenChar input of
  (token, rest) -> if null token
                      then lexer rest
                      else token : lexer rest

isTokenChar :: Char -> Bool
isTokenChar c = not (isSpace c || c `elem` ";()=<>+-*")

isNumeric :: String -> Bool
isNumeric s = all (\c -> c `elem` "0123456789") s

isVar :: String -> Bool
isVar s = not (null s) && all (\c -> isAlphaNum c || c == '_') s

-- parse :: String -> Program
parse :: String -> [Stm]
parse input = buildData (lexer input)

-- Helper function to build data from tokens
buildData :: [String] -> [Stm]
buildData [] = []
buildData ("while":rest) = parseWhile rest
buildData ("if":rest) = parseIf rest
buildData (var:":=":rest) = Assign var (parseAexp rest) : buildData rest
buildData _ = error "Invalid syntax"

-- Helper function to parse arithmetic expressions
parseAexp :: [String] -> Aexp
parseAexp tokens = parseAddA (parseMultA tokens)

parseMultA :: [String] -> Aexp
parseMultA (tok:toks)
  | tok == "(" = parseAexp toks
  | isNumeric tok = Num (read tok)
  | isVar tok = Var tok
  | otherwise = error "Invalid arithmetic expression"

parseAddA :: Aexp -> Aexp
parseAddA a1 = foldl parseTail a1 . concat . chunksOf 2
  where
    parseTail :: Aexp -> [String] -> Aexp
    parseTail a ("+":tok:_) = AddA a (parseMultA [tok])
    parseTail a ("-":tok:_) = SubA a (parseMultA [tok])
    parseTail a _ = a

-- Helper function to parse boolean expressions
parseBexp :: [String] -> Bexp
parseBexp tokens = parseAndB (parseEqA tokens)

parseEqA :: [String] -> Bexp
parseEqA (tok:toks)
  | tok == "(" = parseBexp toks
  | isVar tok = EqA (parseAexp [tok]) (parseAexp toks)
  | isNumeric tok = EqA (parseAexp [tok]) (parseAexp toks)
  | otherwise = error "Invalid boolean expression"

parseAndB :: Bexp -> Bexp
parseAndB b1 = foldl parseTail b1 . concat . chunksOf 2
  where
    parseTail :: Bexp -> [String] -> Bexp
    parseTail b ("and":tok:_) = AndB b (parseEqA [tok])
    parseTail b _ = b

-- Helper function to parse while statements
parseWhile :: [String] -> Stm
parseWhile ("not":rest) = error "Invalid syntax: 'not' not allowed in while condition"
parseWhile (tok:toks) = While (parseBexp [tok]) (buildData toks)

-- Helper function to parse if statements
parseIf :: [String] -> Stm
parseIf ("not":rest) = error "Invalid syntax: 'not' not allowed in if condition"
parseIf (tok:toks) = If (parseBexp [tok]) (buildData toksTrue) (buildData toksFalse)
  where
    (toksTrue, toksFalse) = splitOn "else" toks

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
-}
-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List (intercalate)
import Data.Char (isSpace, isAlphaNum)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha)
-- import Compiler(Aexp(..), Bexp(..), Stm(..), Program)
-- import Interpreter (Inst(..), Code)

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

data StackElement = IntElement Integer | BoolElement Bool
  deriving (Show, Eq)

-- Add the following definition for Stack
type Stack = [StackElement]

-- createEmptyStack :: Stack
-- (returns an empty machine’s stack)
createEmptyStack :: Stack
createEmptyStack = []

-- stack2Str :: Stack -> String
-- (converts a stack given as input to a string.
-- The string represents the stack as an ordered list of values, separated by commas
-- and without spaces, with the leftmost value representing the top of the stack.)
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = showValue x
stack2Str (x:xs) = showValue x ++ "," ++ stack2Str xs

showValue :: StackElement -> String
showValue (IntElement x) = show x
showValue (BoolElement x) = show x

-- Add the following definition for State
newtype State = State { getState :: Map.Map String StackElement }
  deriving (Show, Eq)

-- createEmptyState :: State
-- (returns an empty machine’s state)
createEmptyState :: State
createEmptyState = State Map.empty

-- Inserting a value into the state and mapping it to a key
insertValue :: String -> StackElement -> State -> State
insertValue key value (State state) = State $ Map.insert key value state

-- Reading a value mapped from a key in the state
readValue :: String -> State -> StackElement
readValue key (State state) =
  case Map.lookup key state of
    Just value -> value
    Nothing    -> error "Run-time error"

-- Atualização da função state2Str
-- ( The string represents the state as an list of pairs variable-value, separated
-- by commas and without spaces, with the pairs ordered in alphabetical order of the
-- variable name. Each variable-value pair is represented without spaces and using an
-- ”=”)
state2Str :: State -> String
state2Str (State state) = intercalate "," $ map (\(key, value) -> key ++ "=" ++ showValue value) (Map.toList state)

-- function that based on the instruction given updates the stack and the state accordingly
interpret :: Inst -> (Code, Stack, State) -> (Code, Stack, State)

-- Push Integer
interpret (Push x) (code, stack, state) = 
  (code, IntElement x : stack, state)

-- Add 
interpret Add (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x+y) : xs, state)
interpret Add (code, stack, state) = error $ "Run-time error"

-- Mult
interpret Mult (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x*y) : xs, state)
interpret Mult (code, stack, state) = error $ "Run-time error"

-- Sub
interpret Sub (code, IntElement x : IntElement y : xs, state) = 
  (code, IntElement (x-y) : xs, state)
interpret Sub (code, stack, state) = error $ "Run-time error"

-- Tru
interpret Tru (code, stack, state) = 
  (code, BoolElement True : stack, state)

-- Fals
interpret Fals (code, stack, state) = 
  (code, BoolElement False : stack, state)

-- Equ
interpret Equ (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interpret Equ (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x == y) : xs, state)
interpret Equ (code, stack, state) = error $ "Run-time error"

-- Le
interpret Le (code, IntElement x : IntElement y : xs, state) = 
  (code, BoolElement (x <= y) : xs, state)
interpret Le (code, stack, state) 
  = error $ "Run-time error"

-- And
interpret And (code, BoolElement x : BoolElement y : xs, state) = 
  (code, BoolElement (x && y) : xs, state)
interpret And (code, stack, state) = error $ "Run-time error"

-- Neg
interpret Neg (code, BoolElement x : xs, state) = 
  (code, BoolElement (not x) : xs, state)
interpret Neg (code, stack, state) = error $ "Run-time error"

-- Fetch String
interpret (Fetch key) (code, stack, state)
  | valueIsInt = (code, value : stack, state)
  | valueIsBool = (code, value : stack, state)
  | otherwise = error "Run-time error"
  where
    value = readValue key state
    valueIsInt = case value of
      IntElement _ -> True
      _ -> False
    valueIsBool = case value of
      BoolElement _ -> True
      _ -> False

-- Store String
interpret (Store key) (code, value : stack, state) = 
  (code, stack, insertValue key value state)

-- Noop
interpret Noop (code, stack, state) = 
  (code, stack, state)

-- Branch Code Code
interpret (Branch code1 code2) (code, BoolElement x : xs, state)
  | x = (code1 ++ code, xs, state)
  | otherwise = (code2 ++ code, xs, state)
interpret (Branch code1 code2) (code, stack, state) = error $ "Run-time error"

-- Loop Code Code
interpret (Loop cond code1) (code, stack, state) =
  (cond ++ [Branch (code1 ++ [Loop cond code1]) [Noop]], stack, state)

-- run :: (Code, Stack, State) -> (Code, Stack, State)
-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (x:xs, stack, state) = run (newCode, newStack, newState)
  where (newCode, newStack, newState) = interpret x (xs, stack, state)



-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)


-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","") Pass
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False") Pass
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","") Pass
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","") Pass
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","") Pass
-- testAssembler [Push (-20),Push (-21), Le] == ("True","") Pass
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- Definition of the Arithmetic expressions
data Aexp = Num Integer
          | Var String
          | AddExp Aexp Aexp
          | SubExp Aexp Aexp
          | MultExp Aexp Aexp
          deriving (Show)

-- Definition of the Boolean expressions
data Bexp = TrueExp
          | FalseExp
          | EquExp Aexp Aexp -- Equality
          | EquBoolExp Bexp Bexp  -- Boolean equality
          | LeExp Aexp Aexp
          | AndExp Bexp Bexp
          | NotExp Bexp
          deriving (Show)

-- Definition of the Statements expressions
data Stm = Assign String Aexp
         | If Bexp Stm Stm
         | While Bexp Stm
         | Seq Stm Stm -- Sequence: stm1; stm2
         deriving (Show)

-- Definition of the Program type
type Program = [Stm]

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA (Num n)         = [Push n]
compA (Var x)         = [Fetch x]
compA (AddExp x y)    = compA y ++ compA x ++ [Add]
compA (SubExp x y)    = compA y ++ compA x ++ [Sub]
compA (MultExp x y)   = compA y ++ compA x ++ [Mult]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB TrueExp           = [Tru]
compB FalseExp          = [Fals]
compB (EquExp x y)      = compA y ++ compA x ++ [Equ]
compB (EquBoolExp x y)  = compB y ++ compB x ++ [Equ]
compB (LeExp x y)       = compA y ++ compA x ++ [Le]
compB (AndExp x y)      = compB y ++ compB x ++ [And]
compB (NotExp x)        = compB x ++ [Neg] 

-- Compiles a program into a list of instructions
-- Main function of this module
compile :: Program -> Code
compile [] = []
compile (stm:stms) = compileStm stm ++ compile stms

-- Compiles a individual statement into a list of instructions
compileStm :: Stm -> Code
compileStm (Assign x a)         = compA a ++ [Store x]
compileStm (Seq stm1 stm2)      = compileStm stm1 ++ compileStm stm2
compileStm (While cond stm)     = Loop (compB cond) (compileStm stm) : []
compileStm (If cond stm1 stm2)  = compB cond ++ [Branch (compileStm stm1) (compileStm stm2)]


-- Definition of the Tokens
data Token = TInt Integer
           | TVar String
           | TPlus
           | TMinus
           | TMult
           | TLParen
           | TRParen
           | TTrue
           | TFalse
           | TEquBool
           | TEqu
           | TLe
           | TAnd
           | TNot
           | TAssign
           | TSeq
           | TIf
           | TThen
           | TElse
           | TWhile
           | TDo
           deriving (Show, Eq)

-- Lexing a string into a list of tokens
lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)
  | isDigit c = let (num, restNum) = span isDigit input
                in TInt (read num) : lexer restNum
  | isAlpha c = case keyword of
                  "and" -> TAnd : lexer restKeyword
                  "not" -> TNot : lexer restKeyword
                  "True" -> TTrue : lexer restKeyword
                  "False" -> TFalse : lexer restKeyword
                  "if" -> TIf : lexer restKeyword
                  "then" -> TThen : lexer restKeyword
                  "while" -> TWhile : lexer restKeyword
                  "do" -> TDo : lexer restKeyword
                  "else" -> TElse : lexer restKeyword
                  _     -> TVar var : lexer restVar
  | c == '+' = TPlus : lexer cs
  | c == '-' = TMinus : lexer cs
  | c == '*' = TMult : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | c == '=' && not (null cs) && c2 == '=' = TEqu : lexer newCs
  | c == '<' && not (null cs) && c2 == '=' = TLe : lexer newCs
  | c == '=' = TEquBool : lexer cs
  | c == ':' && not (null cs) && c2 == '=' = TAssign : lexer newCs
  | c == ';' = TSeq : lexer cs
  | otherwise = lexer cs -- Ignoring spaces, for example
  where (var, restVar) = span isAlpha input
        (keyword, restKeyword) = span isAlpha input
        (c2:newCs) = cs
        (followWord, restFollowWord) = span isAlpha newCs


-- Main function to parse a program
parse :: String -> Program
parse program = parseStm (lexer program)

-- Helper function returning a tuple (ExtractedCode, RemaingCode)
getCode :: [Token] -> ([Token], [Token])
getCode (TLParen : t) = do
    let (extractedCode, remaining) = getCodeToRParen t [] [TLParen]
    (extractedCode, remaining)
getCode t = (takeWhile (/= TSeq) t, drop 1 $ dropWhile (/= TSeq) t)

-- Recursively extract code until the matching right parenthesis is found
getCodeToRParen :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
getCodeToRParen [] code stack = error "Run-time error"
getCodeToRParen (TLParen : remaining) code stack = getCodeToRParen remaining (code ++ [TLParen]) (TLParen:stack)
getCodeToRParen (TRParen : remaining) code (TLParen:[]) = (code, remaining)
getCodeToRParen (TRParen : remaining) code (TLParen:stack) = getCodeToRParen remaining (code ++ [TRParen]) stack
getCodeToRParen (x : remaining) code stack = getCodeToRParen remaining (code ++ [x]) stack

-- Recursively parse statements
parseStm :: [Token] -> Program
parseStm [] = []

parseStm (TIf : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code1 = joinStms (parseStm extractedCode1)
                code2 = joinStms (parseStm extractedCode2)
            in If parsedCond code1 code2 : parseStm remaining
        _ -> error "Run-time error"
    where cond = takeWhile (/= TThen) t
          -- [(...) TThen (code1) TElse (code2)]
          afterThen = drop 1 $ dropWhile (/= TThen) t
          beforeElse = takeWhile (/= TElse) afterThen
          (extractedCode1, _) = getCode beforeElse -- note: (...) then stm1; stm2; else (...) Without (), stm2 will be ignored
          afterElse = drop 1 $ dropWhile (/= TElse) t
          (extractedCode2, remaining) = getCode afterElse

parseStm (TWhile : t) =
    case parseBexp cond of
        Just (parsedCond, []) ->
            let code = joinStms (parseStm extractedCode)
            in While parsedCond code : parseStm remaining
        _ -> error "Run-time error"
    where cond = takeWhile (/= TDo) t
          -- [(...) TDo (code)]
          afterDo = drop 1 $ dropWhile (/= TDo) t
          (extractedCode, remaining) = getCode afterDo

parseStm (TVar var : TAssign : t) =
    case parseAexp t of
        Just (parsedExpr, remaining) -> Assign var parsedExpr : parseStm remaining
        _ -> error "Parsing arithmetic expression failed"

parseStm (TSeq : t) = parseStm t

parseStm x = error $ "Run-time error"

-- Helper function to join a list of statements into a single statement
joinStms :: [Stm] -> Stm
joinStms [] = error "Run-time error"
joinStms [stm] = stm
joinStms (stm:stms) = Seq stm (joinStms stms)

-- Helper function to parse arithmetic expressions
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = do
  (left, rest) <- parseAexpOp tokens
  parseAexpInReverse left rest

-- Helper function to parse arithmetic expressions in reverse order
parseAexpInReverse :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseAexpInReverse left [] = return (left, [])
parseAexpInReverse left (TPlus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (AddExp left right) remaining
parseAexpInReverse left (TMinus : t) = do
  (right, remaining) <- parseAexpOp t
  parseAexpInReverse (SubExp left right) remaining
parseAexpInReverse left t = return (left, t)

-- Helper function to parse arithmetic expressions with operators
parseAexpOp :: [Token] -> Maybe (Aexp, [Token])
parseAexpOp tokens = do
  (left, rest) <- parseAexpTerm tokens
  case rest of
    (TMult : t) -> do
      (right, remaining) <- parseAexpOp t
      return (MultExp left right, remaining)
    _ -> return (left, rest)

-- Helper function to parse arithmetic expressions without operators
parseAexpTerm :: [Token] -> Maybe (Aexp, [Token])
parseAexpTerm (TLParen : tokens) =
  case parseAexp tokens of
    Just (exp, TRParen : restTokens) -> Just (exp, restTokens)
    _ -> Nothing
parseAexpTerm (TInt n : restTokens) = Just (Num n, restTokens)
parseAexpTerm (TVar x : restTokens) = Just (Var x, restTokens)
parseAexpTerm _ = Nothing

-- Helper function to parse boolean expressions
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = parseAndExp tokens

-- Helper function to parse boolean expressions with AND operator
parseAndExp :: [Token] -> Maybe (Bexp, [Token])
parseAndExp tokens = do
  (left, rest) <- parseEquBoolExp tokens
  parseAndExpInReverse left rest

-- Helper function to parse boolean expressions with AND operator in reverse order
parseAndExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseAndExpInReverse left [] = return (left, [])
parseAndExpInReverse left (TAnd : t) = do
  (right, remaining) <- parseEquBoolExp t
  parseAndExpInReverse (AndExp left right) remaining
parseAndExpInReverse left t = return (left, t)

-- Helper function to parse boolean expressions with EQUALITY operator
parseEquBoolExp :: [Token] -> Maybe (Bexp, [Token])
parseEquBoolExp tokens = do
  (left, rest) <- parseBasicBexp tokens
  parseEquBoolExpInReverse left rest

-- Helper function to parse boolean expressions with EQUALITY operator in reverse order
parseEquBoolExpInReverse :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseEquBoolExpInReverse left [] = return (left, [])
parseEquBoolExpInReverse left (TEquBool : t) = do
  (right, remaining) <- parseBasicBexp t
  parseEquBoolExpInReverse (EquBoolExp left right) remaining
parseEquBoolExpInReverse left t = return (left, t)

-- Helper function to parse basic boolean expressions
parseBasicBexp :: [Token] -> Maybe (Bexp, [Token])
parseBasicBexp (TTrue : tokens) = Just (TrueExp, tokens)
parseBasicBexp (TFalse : tokens) = Just (FalseExp, tokens)
parseBasicBexp (TNot : tokens) = do
  (exp, rest) <- parseBasicBexp tokens
  return (NotExp exp, rest)
parseBasicBexp (TLParen : tokens) = 
  case parseBexp tokens of
    Just (exp, TRParen : restTokens) -> Just (exp, restTokens)
    _ -> Nothing
parseBasicBexp tokens = parseRelationalAexp tokens

-- Helper function to parse relational expressions
parseRelationalAexp :: [Token] -> Maybe (Bexp, [Token])
parseRelationalAexp tokens = do
  (left, rest) <- parseAexp tokens
  case rest of
    (TEqu : t) -> do
      (right, remaining) <- parseAexp t
      return (EquExp left right, remaining)
    (TLe : t) -> do
      (right, remaining) <- parseAexp t
      return (LeExp left right, remaining)
    _ -> Nothing -- Invalid relational expression


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
-}
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

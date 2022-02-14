{- CS 152 Homework 7:  Boba 2.0
   Team Name: Binary Beasts -}
module Boba2 where -- do not remove


data Token = OpenParen
           | CloseParen
           | Operator Char
           | PosNum Double
           | Let
           | Lambda
           | Identifier String
    deriving (Show, Eq)


data ExpTree = NumNode Double
             | OpNode Char [ExpTree]
             | IdentNode String
             | LetNode String ExpTree ExpTree
             | FunctionNode String ExpTree  -- one formal parameter only
             | Application ExpTree ExpTree -- First ExpTree is a FunctionNode
        deriving (Show, Eq)
        
-- Helper function from boba1 for step 1 to determine if current character is a double
isDigitOrDot :: Char -> Bool
isDigitOrDot x
      | x == '.' = True
      | elem x "1234567890" = True
      | otherwise = False

-- Helper function for step 1 to determine if current character is a valid start to an Identifier
isIdentifier :: Char -> Bool
isIdentifier c = if elem c ['a'..'z'] || elem c ['A'..'Z'] || c == '_'
            then True
            else False

-- STEP 1
keywords = ["let", "lambda"]

scan :: String ->[Token]
scan [] = []
scan xs
      | fst lexeme == head keywords = Let : scan (snd lexeme)
      | fst lexeme == head (tail keywords) = Lambda : scan (snd lexeme)
      | elem y "+-*/" = Operator y : scan (tail (fst lexeme) ++ snd lexeme)
      | isDigitOrDot y = PosNum (read(fst lexeme)::Double) : scan (snd lexeme)
      | isIdentifier y = Identifier (fst lexeme) : scan (snd lexeme)
      | y == '(' = OpenParen : scan (tail (fst lexeme) ++ snd lexeme)
      | y == ')' = CloseParen : scan (tail (fst lexeme) ++ snd lexeme)
      | otherwise = error $ "Lexical Error - invalid character: " ++ [y]
   where lexeme = head (lex xs)
         y = head (fst lexeme)

-- STEP 2: uncomment each step as you work on it

-- parse is the same from boba1
parse :: [Token] -> ExpTree
parse xs = let (x, ys) = expr xs
            in case (x, ys) of
               (_, []) -> x
               (_, _) -> error ("Parse Error - extra tokens: " ++ show ys)

-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN |  POSNUMBER
-- <expr> -> OPENPAREN OPERATOR <operands> CLOSEPAREN
--      |    POSNUMBER
--      |    OPENPAREN LET IDENTIFIER <expr> <expr> CLOSEPAREN |
--      |    <application>
--      |    IDENTIFIER
expr :: [Token] -> (ExpTree, [Token])
expr (OpenParen: Operator c : xs) = 
   let (y, ys) = operands xs
      in case (y, ys) of
         ([], _) -> error ("Parse Error: invalid expression " ++ show xs)
         (_, CloseParen:ws) -> (OpNode c y, ws)
         (_, _) -> error ("Parse Error: invalid expression " ++ show xs)
expr (PosNum x:xs) = (NumNode x, xs)
expr (OpenParen: Let: Identifier x: xs) = let (y, ys) = expr xs
                                              (w, ws) = expr ys
                                                in case ws of
                                                CloseParen : vs -> (LetNode x y w, vs)
                                                _ -> error ("Parse Error: invalid expression " ++ show xs)
expr (Identifier x:xs) = (IdentNode x, xs)
expr xs = application xs

-- <operands> ->  <expr> [<operands>]
operands :: [Token] -> ([ExpTree], [Token])
operands xs = let (y, ys) = expr xs
                in case ys of
                OpenParen : ws -> let (z, zs) = operands ys
                                  in (y:z, zs)
                PosNum w: ws -> let (v, vs) = operands ys
                                 in (y:v, vs)
                Identifier w: ws -> let (u, us) = operands ys
                                     in (y:u, us)
                _ -> ([y], ys)

-- <function> -> OPENPAREN LAMBDA OPENPAREN IDENTIFIER CLOSEPAREN <expr> CLOSEPAREN
function :: [Token] -> (ExpTree, [Token])
function (OpenParen: Lambda: OpenParen: Identifier x: CloseParen: xs) = let (y, ys) = expr xs
                                                                              in case ys of
                                                                              CloseParen : ws -> (FunctionNode x y, ws)
                                                                              _ -> error ("Invalid function: " ++ show xs)
function xs = error ("Invalid function: " ++ show xs)

-- <application>	->  OPENPAREN <function> <expression> CLOSEPAREN
application :: [Token] -> (ExpTree, [Token])
application (OpenParen: xs) = let (y, ys) = function xs
                                  (w, ws) = expr ys
                                    in case ws of
                                    CloseParen: vs -> (Application y w, vs)
                                    _ -> error ("Parse Error: invalid function application: " ++ show ws)
application xs = error ("Parse Error: invalid function application: " ++ show xs)

stringToTree:: String -> ExpTree
stringToTree = parse.scan -- for testing convenience


-- STEP 3: uncomment each step as you work on it
eval :: [(String, Double)] -> ExpTree -> Double
--cases for handling OpNodes 
eval env (NumNode x) = x
eval env (OpNode x xs) = 
    case (x, xs) of
        ('+', _) -> foldl1 (+) (map (eval env) xs)
        ('-', [y]) -> - (head (map (eval env) xs))
        ('-', _) -> foldl1 (-) (map (eval env) xs)
        ('*', _) -> foldl1 (*) (map (eval env) xs)
        ('/', [y]) -> 1 / (head (map (eval env) xs))
        ('/', _) -> foldl1 (/) (map (eval env) xs)

--case that handles the identifiers 
eval env (IdentNode x) = case lookup x env of
                            Nothing -> error "*** Exception: Undefined Identifier: x"
                            Just n -> n

--case that handles let functions
eval env (LetNode x y z) = eval ((x, eval env y):env) z

--case that handles lambda functions
eval env (Application (FunctionNode x y) (z)) = eval ((x, eval env z):env) y

--catch all case that handles any invalid formatting 
eval env catchErrors = error ("Incorrect Node Format " ++ show catchErrors) 

eval0 :: ExpTree -> Double
eval0 x = eval [] x -- call eval with an empty environment for eval0 

interpret :: String -> Double
interpret = eval0.parse.scan
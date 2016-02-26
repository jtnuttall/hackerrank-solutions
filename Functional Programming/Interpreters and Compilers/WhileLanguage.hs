{-# OPTIONS_GHC -O2 #-}
-- A parser for while language using parsec
-- https://www.hackerrank.com/challenges/while-language-fp
-- Working implementation

import Control.Monad
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Token as Tk
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Expr as E

type VarTable = M.Map String Integer

data BinOp = And | Or
    deriving (Show)

data CompOp = Greater | Less
    deriving (Show)

data BoolExp = BConst Bool
             | Not BoolExp
             | BBin    BinOp  BoolExp  BoolExp
             | Compare CompOp ArithExp ArithExp
             deriving (Show)

data BinArithOp = Add
                | Subtract
                | Multiply
                | Divide
                deriving (Show)

data ArithExp = Var    String
              | IntVar Integer
              | Negate ArithExp
              | BinArith BinArithOp ArithExp ArithExp
              deriving (Show)

data Statement = Sequence [Statement]
               | Assign String ArithExp
               | If BoolExp Statement Statement
               | While BoolExp Statement
               deriving (Show)

whileLang = 
    L.emptyDef { 
          Tk.commentStart    = "/*"
        , Tk.commentEnd      = "*/"
        , Tk.commentLine     = "//"
        , Tk.identStart      = C.letter
        , Tk.identLetter     = C.alphaNum
        , Tk.reservedNames   = [ "if", "then", "else"
                              , "while", "do"
                              , "true", "false"
                              , "not", "and", "or"
                              ]
        , Tk.reservedOpNames = [ "+", "-", "*", "/"
                              , ":="
                              , "<", ">"
                              , "not", "and", "or"
                              ]
    }

lexer = Tk.makeTokenParser whileLang

identifier = Tk.identifier lexer
reserved   = Tk.reserved   lexer
reservedOp = Tk.reservedOp lexer
braces     = Tk.braces   lexer
parens     = Tk.parens     lexer
integer    = Tk.integer    lexer
semi       = Tk.semi       lexer
whitespace = Tk.whiteSpace lexer

boolOps = 
    [ [E.Prefix (reservedOp "not" >> (return Not))]
    , [ E.Infix (reservedOp "and" >> (return $ BBin And)) E.AssocLeft,
        E.Infix (reservedOp "or"  >> (return $ BBin Or )) E.AssocLeft]
    ]

comparison = do
    first  <- arithExp
    op     <- operator
    second <- arithExp
    return (Compare op first second)
    where
        operator =  (reservedOp ">" >> (return Greater))
                <|> (reservedOp "<" >> (return Less))

boolTerms =  parens boolExp
         <|> (reserved "true"  >> (return $ BConst True ))
         <|> (reserved "false" >> (return $ BConst False))
         <|> comparison

boolExp :: P.Parser BoolExp
boolExp = E.buildExpressionParser boolOps boolTerms

arithOps = 
    [ [E.Prefix (reservedOp "-" >> (return   Negate))]
    , [ E.Infix (reservedOp "*" >> (return $ BinArith Multiply)) E.AssocLeft,
        E.Infix (reservedOp "/" >> (return $ BinArith Divide))   E.AssocLeft ]
    , [ E.Infix (reservedOp "+" >> (return $ BinArith Add))      E.AssocLeft,
        E.Infix (reservedOp "-" >> (return $ BinArith Subtract)) E.AssocLeft ]
    ]

arithTerms =  parens arithExp
          <|> liftM Var identifier
          <|> liftM IntVar integer

arithExp :: P.Parser ArithExp
arithExp = E.buildExpressionParser arithOps arithTerms

ifStatement :: P.Parser Statement
ifStatement = do
    reserved "if"
    condition <- boolExp
    reserved "then"
    whenTrue  <- statement
    reserved "else"
    whenFalse <- statement
    return (If condition whenTrue whenFalse)

whileStatement :: P.Parser Statement
whileStatement = do
    reserved "while"
    condition <- boolExp
    reserved "do"
    doThese   <- statement
    return (While condition doThese)

assignStatement :: P.Parser Statement
assignStatement = do
    var <- identifier
    reservedOp ":="
    val <- arithExp
    return (Assign var val)

statement :: P.Parser Statement
statement =  parens statement
         <|> braces seqStatement
         <|> seqStatement
    where
        seqStatement = do
            cmds <- P.sepBy1 statement' semi
            case cmds of
               [x] -> return x
               _  -> return (Sequence cmds)

            where
                statement' =  ifStatement
                          <|> whileStatement
                          <|> assignStatement

whileParser :: P.Parser Statement
whileParser = whitespace >> statement

evalBool :: BoolExp -> VarTable -> Bool
evalBool (BConst b) _ = b
evalBool (Not    e) m = not (evalBool e m)
evalBool (BBin And e1 e2) m = (evalBool e1 m) && (evalBool e2 m)
evalBool (BBin Or  e1 e2) m = (evalBool e1 m) || (evalBool e2 m)
evalBool (Compare Greater i1 i2) m = (evalInt i1 m) > (evalInt i2 m)
evalBool (Compare Less    i1 i2) m = (evalInt i1 m) < (evalInt i2 m)

evalInt :: ArithExp -> VarTable -> Integer
evalInt (IntVar i) _ = i
evalInt (Var    v) m = m M.! v
evalInt (Negate e) m = negate (evalInt e m)
evalInt (BinArith op e1 e2) m = evalBin op e1 e2 m

evalBin :: BinArithOp -> ArithExp -> ArithExp -> VarTable -> Integer
evalBin Add      e1 e2 m = (evalInt e1 m)   +   (evalInt e2 m)
evalBin Subtract e1 e2 m = (evalInt e1 m)   -   (evalInt e2 m)
evalBin Multiply e1 e2 m = (evalInt e1 m)   *   (evalInt e2 m)
evalBin Divide   e1 e2 m = (evalInt e1 m) `div` (evalInt e2 m)

toSeq :: Statement -> [Statement]
toSeq s = case s of
    Sequence xs -> xs
    x           -> [x]

runProgram :: [Statement] -> VarTable -> VarTable
runProgram [] m = m

runProgram (Assign var val : rest) m = 
    let int = evalInt val m
    in runProgram rest (M.insert var int m)

runProgram (If cond first second : rest) m = do
    let ss = if evalBool cond m
             then toSeq first 
             else toSeq second
    let m' = runProgram ss m

    runProgram rest m'

runProgram (While cond doThis : rest) m =
    let doThese = toSeq doThis
        loop m' | evalBool cond m' = loop (runProgram doThese m')
                | otherwise        = m'
    in runProgram rest (loop m)

main :: IO()
main = do
    input <- getContents
    case P.parse whileParser "" input of
        Left  err -> error (show err)
        Right (Sequence p) -> do
            let result = runProgram p M.empty
            forM_ (M.toList result) $ \(var, val) -> 
                putStrLn (var ++ " " ++ show val)
module Assignment
    ( ADT (..)
    , Declaration (..)
    , Domain (..)
    , Expr (..)
    , Constraint (..)
    , CompareOperator (..)
    , SolverGoal (..)
    , miniZincParser
    , solveProblem
    , getTime
    ) where

import Control.Applicative (many, (<|>))
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import Instances (ParseError (UnexpectedString), Parser)
import Parser (charTok, eof, int, satisfy, spaces, stringTok, tok, is, failed, sepBy, commaTok)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

type Identifier = String

data ADT = ADT
    { declarations :: [Declaration]
    , constraints :: [Constraint]
    , solverGoal :: SolverGoal
    }
    deriving (Eq, Show)

data Declaration
    = ConstantDeclaration Identifier Expr
    | VariableDeclaration Identifier Domain
    deriving (Eq, Show)

data Domain = Range Int Int
    | FiniteSet [Int]
    deriving (Eq, Show)

data Expr
    = IntLit Int
    | Var Identifier
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Neg Expr
    deriving (Eq, Show)

data CompareOperator = OpEq
    deriving (Eq, Show)

data Constraint = Compare CompareOperator Expr Expr
    deriving (Eq, Show)

data SolverGoal = Satisfy
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

miniZincParser :: Parser ADT
miniZincParser = do
    _ <- spaces
    decls <- many declarationParser
    _ <- stringTok "solve"
    _ <- stringTok "satisfy"
    _ <- charTok ';'
    _ <- spaces
    _ <- eof
    pure (ADT decls [] Satisfy)

-- Parse an identifier starting with a letter, followed by letters, digits or underscores
identifierParser :: Parser String
identifierParser = do
    first <- satisfy isAlpha
    rest <- many (satisfy isIdentifierChar)
    _ <- spaces
    pure (first : rest)
  where
    isIdentifierChar c = isAlphaNum c || c == '_'

-- | Parse arithmetic expressions in precedence order.
exprParser :: Parser Expr
exprParser = addSubParser

-- | Parse addition and subtraction from left to right.
addSubParser :: Parser Expr
addSubParser = do
    first <- mulDivParser
    rest <- many $ do
        op <- (Add <$ charTok '+')
          <|> (Sub <$ charTok '-')
        value <- mulDivParser
        pure (op, value)
    pure (foldl applyOp first rest)

-- | Parse multiplication and division.
mulDivParser :: Parser Expr
mulDivParser = do
    first <- unaryParser
    rest <- many $ do
        op <- (Mul <$ charTok '*')
          <|> (Div <$ charTok '/')
        value <- unaryParser
        pure (op, value)
    pure (foldl applyOp first rest)

-- | Apply an operator to the accumulated expression.
applyOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
applyOp left (op, right) = op left right

-- | Parse unary negation after exponentiation precedence.
unaryParser :: Parser Expr
unaryParser =
    (Neg <$> (charTok '-' *> unaryParser))
        <|> powerParser

-- | Parse right-associative exponentiation.
powerParser :: Parser Expr
powerParser = do
    base <- atomParser
    (Pow base <$> (charTok '^' *> unaryParser))
        <|> pure base

-- | Parse literals, identifiers and parenthesized expressions.
atomParser :: Parser Expr
atomParser =
    (charTok '(' *> exprParser <* charTok ')')
        <|> (IntLit <$> tok int)
        <|> (Var <$> identifierParser)

-- Parse positive and negative integer literals.
-- The minus sign is optional, and whitespace after the integer is ignored.
signedIntParser :: Parser Int
signedIntParser = tok $
    (negate <$> (is '-' *> int))
        <|> int

-- Parse a finite set of integer literals.
-- Reject duplicate values within the set.
finiteSetParser :: Parser Domain
finiteSetParser = do
    _ <- charTok '{'
    values <- signedIntParser `sepBy` commaTok
    _ <- charTok '}'

    if length values == length (unique values)
        then pure (FiniteSet values)
        else failed (UnexpectedString "Duplicate set values")
  where
    unique :: Eq a => [a] -> [a]
    unique [] = []
    unique (x:xs)
        | x `elem` xs = unique xs
        | otherwise = x : unique xs

-- Parse an inclusive integer range.
-- Reject ranges whose lower bound exceeds their upper bound.
rangeParser :: Parser Domain
rangeParser = do
    lower <- signedIntParser
    _ <- stringTok ".."
    upper <- signedIntParser

    if lower <= upper
        then pure (Range lower upper)
        else failed (UnexpectedString "Invalid range")

-- Parse either a finite set or an integer range
domainParser :: Parser Domain
domainParser = finiteSetParser <|> rangeParser

-- Parse a variable declaration.
variableDeclarationParser :: Parser Declaration
variableDeclarationParser = do
    _ <- stringTok "var"
    domain <- domainParser
    _ <- charTok ':'
    name <- identifierParser
    _ <- charTok ';'
    pure (VariableDeclaration name domain)

-- Parse an integer constant declaration.
-- Constants can contain integer literals or references to previously declared identifiers
constantDeclarationParser :: Parser Declaration
constantDeclarationParser = do
    _ <- stringTok "int"
    _ <- charTok ':'
    name <- identifierParser
    _ <- charTok '='
    value <- exprParser
    _ <- charTok ';'
    pure (ConstantDeclaration name value)

-- Parse either a constant or variable declaration.
declarationParser :: Parser Declaration
declarationParser =
    constantDeclarationParser
        <|> variableDeclarationParser

--------------------------------------------------------------------------------
-- Solver
--------------------------------------------------------------------------------

type Binding = (Identifier, Int)

data ConstraintProblem = ConstraintProblem
    { searchConstants :: [Binding]
    , solvedVars :: [Binding]
    , remainingDeclarations :: [Declaration]
    , searchConstraints :: [Constraint]
    }

solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt = listToMaybe (search problem)
  where
    problem =
        ConstraintProblem
            { searchConstants = []
            , solvedVars = []
            , remainingDeclarations = declarations adt
            , searchConstraints = constraints adt
            }

search :: ConstraintProblem -> [[Binding]]
search p = case remainingDeclarations p of
    [] -> if allSatisfied p then [reverse (solvedVars p)] else []
    ConstantDeclaration name e : rest ->
        case evalExpr p e of
            Nothing -> []
            Just v ->
                search
                    p
                        { remainingDeclarations = rest
                        , searchConstants = (name, v) : searchConstants p
                        }
    VariableDeclaration name dom : rest -> do
        v <- domainValues p dom
        search
            p
                { remainingDeclarations = rest
                , solvedVars = (name, v) : solvedVars p
                }

-- Generate possible values for a variable domain.
domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues _ (Range lo hi) = [lo..hi]
domainValues _ (FiniteSet values) = values

-- | Evaluate an expression using the current bindings.
-- Invalid operations return Nothing instead of crashing.
evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr _ (IntLit n) = Just n

evalExpr p (Var x) =
    lookup x (searchConstants p)
        <|> lookup x (solvedVars p)

evalExpr p (Add a b) =
    (+) <$> evalExpr p a <*> evalExpr p b

evalExpr p (Sub a b) =
    (-) <$> evalExpr p a <*> evalExpr p b

evalExpr p (Mul a b) =
    (*) <$> evalExpr p a <*> evalExpr p b

evalExpr p (Div a b) = do
    x <- evalExpr p a
    y <- evalExpr p b
    if y == 0
        then Nothing
        else Just (x `div` y)

evalExpr p (Pow a b) = do
    x <- evalExpr p a
    y <- evalExpr p b
    if y < 0
        then Nothing
        else Just (x ^ y)

evalExpr p (Neg a) =
    negate <$> evalExpr p a

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

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

import Control.Applicative (many, some, (<|>))
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import Instances (ParseError (UnexpectedString), Parser)
import Parser (charTok, eof, int, satisfy, spaces, stringTok, tok, is, failed)

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
    deriving (Eq, Show)

data Expr
    = IntLit Int
    | Var Identifier
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
    decls <- many variableDeclarationParser
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

-- Parse positive and negative integer literals.
-- The minus sign is optional, and whitespace after the integer is ignored.
signedIntParser :: Parser Int
signedIntParser = tok $
    (negate <$> (is '-' *> int))
        <|> int

-- Parse an inclusive integer range.
-- Reject ranges whose lower bound exceeds their upper bound.
domainParser :: Parser Domain
domainParser = do
    lower <- signedIntParser
    _ <- stringTok ".."
    upper <- signedIntParser

    if lower <= upper
        then pure (Range lower upper)
        else failed (UnexpectedString "Invalid range")

-- Parse a variable declaration.
variableDeclarationParser :: Parser Declaration
variableDeclarationParser = do
    _ <- stringTok "var"
    domain <- domainParser
    _ <- charTok ':'
    name <- identifierParser
    _ <- charTok ';'
    pure (VariableDeclaration name domain)

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

domainValues :: ConstraintProblem -> Domain -> [Int]
domainValues p (Range lo hi) = [lo..hi]

evalExpr :: ConstraintProblem -> Expr -> Maybe Int
evalExpr p (IntLit n) = Just n
evalExpr p (Var x) = lookup x (searchConstants p) <|> lookup x (solvedVars p)

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b

applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

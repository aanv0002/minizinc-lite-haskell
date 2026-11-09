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
import Data.List (nub)

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

data NumericMacroName
    = MacroSum
    | MacroMin
    | MacroMax
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
    | NumericMacro NumericMacroName [Expr]
    deriving (Eq, Show)

data CompareOperator
    = OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    deriving (Eq, Show)

data Constraint
    = Compare CompareOperator Expr Expr
    | AllConstraints [Constraint]
    | AnyConstraints [Constraint]
    | AllDifferent [Expr]
    deriving (Eq, Show)

-- The objective of a MiniZinc constraint problem.
data SolverGoal
    = Satisfy
    | Minimize Expr
    | Maximize Expr
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- Parse one of the six comparison operators.
compareOperatorParser :: Parser CompareOperator
compareOperatorParser =
        (OpNe <$ stringTok "!=")
    <|> (OpLe <$ stringTok "<=")
    <|> (OpGe <$ stringTok ">=")
    <|> (OpEq <$ stringTok "=")
    <|> (OpLt <$ stringTok "<")
    <|> (OpGt <$ stringTok ">")

-- Parse a comparison or predefined boolean macro.
constraintParser :: Parser Constraint
constraintParser = do
    _ <- stringTok "constraint"

    result <-
            allDifferentParser
        <|> booleanComparisonMacroParser
        <|> comparisonParser

    _ <- charTok ';'
    pure result

-- Parse declarations, constraints and one solve goal.
-- Reject models containing undefined references.
miniZincParser :: Parser ADT
miniZincParser = do
    _ <- spaces

    decls <- declarationsParser [] []
    cons <- many constraintParser
    goal <- solveDirectiveParser

    _ <- spaces
    _ <- eof

    let model = ADT decls cons goal

    if validReferences model
        then pure model
        else failed (UnexpectedString
            "Undefined identifier in model")

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

-- Parse literals, identifiers and parenthesized expressions.
atomParser :: Parser Expr
atomParser =
        numericMacroParser
    <|> (charTok '(' *> exprParser <* charTok ')')
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

-- Parse a literal or a previously defined constant
-- used as a range boundary.
rangeBoundParser :: [Binding] -> Parser Int
rangeBoundParser constants =
        negativeConstant
    <|> signedIntParser
    <|> constantReference
  where
    constantReference = do
        name <- identifierParser
        case lookup name constants of
            Just value -> pure value
            Nothing ->
                failed (UnexpectedString
                    ("Undefined constant: " ++ name))

    negativeConstant = do
        _ <- charTok '-'
        negate <$> constantReference

-- Parse a valid inclusive range using known constants.
rangeParserWith :: [Binding] -> Parser Domain
rangeParserWith constants = do
    lower <- rangeBoundParser constants
    _ <- stringTok ".."
    upper <- rangeBoundParser constants

    if lower <= upper
        then pure (Range lower upper)
        else failed (UnexpectedString "Invalid range")

-- Parse a finite set or constant-aware integer range.
domainParserWith :: [Binding] -> Parser Domain
domainParserWith constants =
    finiteSetParser <|> rangeParserWith constants

-- Parse variable declarations using previously
-- evaluated constants for domain boundaries.
variableDeclarationParserWith
    :: [Binding] -> Parser Declaration
variableDeclarationParserWith constants = do
    _ <- stringTok "var"
    domain <- domainParserWith constants
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

-- | Parse a declaration using previously evaluated constants.
declarationParserWith
    :: [Binding]
    -> Parser (Declaration, Maybe Binding)
declarationParserWith constants =
    parseConstant <|> parseVariable
  where
    parseConstant = do
        decl <- constantDeclarationParser

        case decl of
            ConstantDeclaration name expression ->
                case evaluateConstant constants expression of
                    Just value ->
                        pure (decl, Just (name, value))

                    Nothing ->
                        failed (UnexpectedString
                            ("Invalid constant: " ++ name))

            _ ->
                failed (UnexpectedString
                    "Expected a constant declaration")

    parseVariable = do
        decl <- variableDeclarationParserWith constants
        pure (decl, Nothing)

-- | Parse declarations sequentially while tracking
-- defined constants and all previously used names.
declarationsParser
    :: [Binding]
    -> [Identifier]
    -> Parser [Declaration]
declarationsParser constants usedNames =
        parseNext
    <|> pure []
  where
    parseNext = do
        (decl, newConstant) <-
            declarationParserWith constants

        let name = case decl of
                ConstantDeclaration n _ -> n
                VariableDeclaration n _ -> n

        if name `elem` usedNames
            then failed (UnexpectedString
                ("Duplicate identifier: " ++ name))
            else do
                let updatedConstants =
                        case newConstant of
                            Just binding ->
                                binding : constants
                            Nothing ->
                                constants

                rest <- declarationsParser
                    updatedConstants
                    (name : usedNames)

                pure (decl : rest)

-- Evaluate a constant using only earlier constants.
-- Decision variables are intentionally unavailable.
evaluateConstant :: [Binding] -> Expr -> Maybe Int
evaluateConstant constants expression =
    evalExpr problem expression
  where
    problem = ConstraintProblem
        { searchConstants = constants
        , solvedVars = []
        , remainingDeclarations = []
        , searchConstraints = []
        }

-- Parse satisfaction and optimization directives.
solveDirectiveParser :: Parser SolverGoal
solveDirectiveParser = do
    _ <- stringTok "solve"

    goal <-
            (Satisfy <$ stringTok "satisfy")
        <|> (Minimize <$> (stringTok "minimize" *> exprParser))
        <|> (Maximize <$> (stringTok "maximize" *> exprParser))

    _ <- charTok ';'
    pure goal

-- Parse a list of expressions enclosed in brackets.
expressionListParser :: Parser [Expr]
expressionListParser =
    charTok '['
        *> (exprParser `sepBy` commaTok)
        <* charTok ']'

-- Parse a numeric macro: sum, min or max.
numericMacroParser :: Parser Expr
numericMacroParser = do
    macro <-
            (MacroSum <$ stringTok "sum")
        <|> (MacroMin <$ stringTok "min")
        <|> (MacroMax <$ stringTok "max")

    _ <- charTok '('
    values <- expressionListParser
    _ <- charTok ')'

    pure (NumericMacro macro values)

-- Parse a comparison without the constraint keyword.
comparisonParser :: Parser Constraint
comparisonParser = do
    left <- exprParser
    op <- compareOperatorParser
    right <- exprParser
    pure (Compare op left right)

-- Parse an array of comparison constraints.
comparisonListParser :: Parser [Constraint]
comparisonListParser =
    charTok '['
        *> (comparisonParser `sepBy` commaTok)
        <* charTok ']'

-- Parse boolean macros containing comparisons.
booleanComparisonMacroParser :: Parser Constraint
booleanComparisonMacroParser = do
    constructor <-
            (AllConstraints <$ stringTok "all")
        <|> (AnyConstraints <$ stringTok "any")

    _ <- charTok '('
    comparisons <- comparisonListParser
    _ <- charTok ')'

    pure (constructor comparisons)

-- Parse allDifferent over a list of expressions.
allDifferentParser :: Parser Constraint
allDifferentParser = do
    _ <- stringTok "allDifferent"
    _ <- charTok '('
    values <- expressionListParser
    _ <- charTok ')'
    pure (AllDifferent values)

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

-- Find a satisfying assignment or the optimal assignment.
solveProblem :: ADT -> Maybe [(String, Int)]
solveProblem adt =
    case solverGoal adt of
        Satisfy ->
            listToMaybe solutions

        Minimize objective ->
            chooseOptimal (<) objective solutions constants

        Maximize objective ->
            chooseOptimal (>) objective solutions constants
  where
    solutions = search (initialProblem adt)
    constants = evaluateConstants (declarations adt)

-- Evaluate constants in declaration order.
evaluateConstants :: [Declaration] -> Maybe [Binding]
evaluateConstants = go []
  where
    go bindings [] = Just bindings

    go bindings (ConstantDeclaration name expr : rest) = do
        let p = ConstraintProblem
                { searchConstants = bindings
                , solvedVars = []
                , remainingDeclarations = []
                , searchConstraints = []
                }

        value <- evalExpr p expr
        go ((name, value) : bindings) rest

    go bindings (_ : rest) =
        go bindings rest

-- Select the best solution for a given objective.
chooseOptimal
    :: (Int -> Int -> Bool)
    -> Expr
    -> [[Binding]]
    -> Maybe [Binding]
    -> Maybe [Binding]
chooseOptimal better objective solutions constants =
    case constants of
        Nothing -> Nothing
        Just constantBindings ->
            fmap snd (foldl selectBest Nothing scored)
          where
            scored =
                [ (value, bindings)
                | bindings <- solutions
                , let p = ConstraintProblem
                        { searchConstants = constantBindings
                        , solvedVars = bindings
                        , remainingDeclarations = []
                        , searchConstraints = []
                        }
                , Just value <- [evalExpr p objective]
                ]

            selectBest Nothing candidate = Just candidate
            selectBest (Just best@(bestValue, _))
                       candidate@(value, _)
                | better value bestValue = Just candidate
                | otherwise = Just best

-- Create the initial search state from a parsed model.
initialProblem :: ADT -> ConstraintProblem
initialProblem adt =
    ConstraintProblem
        { searchConstants = []
        , solvedVars = []
        , remainingDeclarations = declarations adt
        , searchConstraints = constraints adt
        }

-- Collect identifiers referenced by an expression.
exprIdentifiers :: Expr -> [Identifier]
exprIdentifiers (IntLit _) = []
exprIdentifiers (Var name) = [name]
exprIdentifiers (Neg a) = exprIdentifiers a
exprIdentifiers (Add a b) = binaryIdentifiers a b
exprIdentifiers (Sub a b) = binaryIdentifiers a b
exprIdentifiers (Mul a b) = binaryIdentifiers a b
exprIdentifiers (Div a b) = binaryIdentifiers a b
exprIdentifiers (Pow a b) = binaryIdentifiers a b
exprIdentifiers (NumericMacro _ values) =
    concatMap exprIdentifiers values

-- Collect identifiers from two expressions.
binaryIdentifiers :: Expr -> Expr -> [Identifier]
binaryIdentifiers a b =
    exprIdentifiers a ++ exprIdentifiers b

-- Collect identifiers used by each constraint form.
constraintIdentifiers :: Constraint -> [Identifier]
constraintIdentifiers (Compare _ a b) =
    exprIdentifiers a ++ exprIdentifiers b

constraintIdentifiers (AllConstraints comparisons) =
    concatMap constraintIdentifiers comparisons

constraintIdentifiers (AnyConstraints comparisons) =
    concatMap constraintIdentifiers comparisons

constraintIdentifiers (AllDifferent expressions) =
    concatMap exprIdentifiers expressions

-- Extract identifiers from an optimization objective.
goalIdentifiers :: SolverGoal -> [Identifier]
goalIdentifiers Satisfy = []
goalIdentifiers (Minimize expression) =
    exprIdentifiers expression
goalIdentifiers (Maximize expression) =
    exprIdentifiers expression

-- Ensure constraint and objective identifiers
-- refer to declarations in the parsed model.
validReferences :: ADT -> Bool
validReferences model =
    all (`elem` declaredNames) referencedNames
  where
    declaredNames =
        map declarationName (declarations model)

    referencedNames =
        concatMap constraintIdentifiers
            (constraints model)
        ++ goalIdentifiers (solverGoal model)

    declarationName (ConstantDeclaration name _) = name
    declarationName (VariableDeclaration name _) = name

-- | Determine whether all identifiers have values.
constraintReady :: ConstraintProblem -> Constraint -> Bool
constraintReady p c =
    all isBound (constraintIdentifiers c)
  where
    bindings = searchConstants p ++ solvedVars p

    isBound name =
        name `elem` map fst bindings

-- Reject constraints that are already known to be false.
-- Constraints with missing variables are checked later.
consistent :: ConstraintProblem -> Bool
consistent p = all valid (searchConstraints p)
  where
    valid c
        | constraintReady p c =
            evalConstraint p c == Just True
        | otherwise = True

-- Search variable assignments using incremental backtracking.
search :: ConstraintProblem -> [[Binding]]
search p =
    case remainingDeclarations p of
        [] ->
            if allSatisfied p
                then [reverse (solvedVars p)]
                else []

        ConstantDeclaration name e : rest ->
            case evalExpr p e of
                Nothing -> []
                Just value ->
                    let next = p
                            { remainingDeclarations = rest
                            , searchConstants =
                                (name, value) : searchConstants p
                            }
                    in if consistent next
                        then search next
                        else []

        VariableDeclaration name dom : rest -> do
            value <- domainValues p dom

            let next = p
                    { remainingDeclarations = rest
                    , solvedVars =
                        (name, value) : solvedVars p
                    }

            if consistent next
                then search next
                else []

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

evalExpr p (NumericMacro macro values) = do
    numbers <- traverse (evalExpr p) values
    case macro of
        MacroSum -> Just (sum numbers)
        MacroMin ->
            if null numbers
                then Nothing
                else Just (minimum numbers)
        MacroMax ->
            if null numbers
                then Nothing
                else Just (maximum numbers)

evalConstraint :: ConstraintProblem -> Constraint -> Maybe Bool
evalConstraint p (Compare op a b) = applyCompare op <$> evalExpr p a <*> evalExpr p b

evalConstraint p (AllConstraints comparisons) =
    all id <$> traverse (evalConstraint p) comparisons

evalConstraint p (AnyConstraints comparisons) =
    any id <$> traverse (evalConstraint p) comparisons

evalConstraint p (AllDifferent expressions) = do
    values <- traverse (evalExpr p) expressions
    pure (length values == length (nub values))

-- Evaluate a comparison between two integer values.
applyCompare :: CompareOperator -> Int -> Int -> Bool
applyCompare OpEq = (==)
applyCompare OpNe = (/=)
applyCompare OpLt = (<)
applyCompare OpLe = (<=)
applyCompare OpGt = (>)
applyCompare OpGe = (>=)

allSatisfied :: ConstraintProblem -> Bool
allSatisfied p = all ((== Just True) . evalConstraint p) (searchConstraints p)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

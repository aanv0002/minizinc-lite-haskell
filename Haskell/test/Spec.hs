{-# LANGUAGE TypeApplications #-}

import Assignment
import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

import Instances (ParseResult (Error, Result), parse)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath
    ( joinPath
    , replaceExtension
    , splitDirectories
    , splitFileName
    , takeBaseName
    , takeFileName
    , (</>)
    )
import System.FilePath.Glob (glob)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

-- | A result from 'solveProblem': either a single solution or 'Nothing'.
type Solution = Maybe [(String, Int)]

-- | Normalise the order of bindings (alphabetical by variable name)
normalise :: Solution -> Solution
normalise = fmap (sortOn fst)

-- | Parse one line of an expected output file. Each line is either the word
-- @Nothing@ (no solution) or a shown binding list such as
-- @[("x",2),("y",1)]@.
readExpected :: String -> Maybe Solution
readExpected line
    | trim line == "Nothing" = Just Nothing
    | otherwise = Just <$> readMaybe (trim line)
  where
    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Does @solution@ match any of the accepted answers?
isAccepted :: [String] -> Solution -> Bool
isAccepted expectedLines solution =
    any (== normalise solution) accepted
  where
    accepted = mapMaybe (fmap normalise . readExpected) expectedLines

-- Example Model (Parser and Solver)
exampleModel :: ADT
exampleModel =
    ADT
        { declarations =
            [VariableDeclaration "x" (Range 1 3)]
        , constraints = [Compare OpEq (Var "x") (IntLit 2)]
        , solverGoal = Satisfy
        }

checkADTExampleModel :: IO ()
checkADTExampleModel = do
    let model = "var 1..3: x; constraint x = 2; solve satisfy;"
    putStrLn "Testing ADT's Show instance:"
    putStrLn $ "Input string: " ++ model
    putStrLn $ case parse miniZincParser model of
        Error _ -> "Parse error"
        Result _ adt -> "Result: " ++ show adt ++ "\n" ++ "Eq: " ++ show ((==) adt exampleModel)

checkSolverExampleModel :: IO ()
checkSolverExampleModel = do
    putStrLn "Testing Solver with example model"
    putStrLn $ "> " ++ show (solveProblem exampleModel) ++ " <"
    putStrLn $ "> expected Just [(\"x\", 2)]"

mkFolderIfNotExists :: FilePath -> IO ()
mkFolderIfNotExists = createDirectoryIfMissing True

genOutputName :: FilePath -> String -> String -> FilePath
genOutputName outputFolder extension file = replaceExtension (replaceFolderAtIndex file 1 outputFolder) extension

replaceFolderAtIndex :: FilePath -> Int -> String -> FilePath
replaceFolderAtIndex filePath index newFolder =
    joinPath (replaceAtIndex index newFolder (splitDirectories dirPath))
        </> fileName
  where
    -- Split the file path into directory and file components
    dirPath = fst $ splitFileName filePath
    fileName = takeFileName filePath

    -- Replace the folder at the specified index using zipWith
    replaceAtIndex :: Int -> String -> [FilePath] -> [FilePath]
    replaceAtIndex idx newVal = zipWith (\i folder -> if i == idx then newVal else folder) [0 ..]

-- | The expected output file a given input is checked against.
expectedPath :: FilePath -> FilePath
expectedPath inputFile = "examples/expected_output/" ++ takeBaseName inputFile ++ ".out"

-- | Parse and solve one input file, writing the result to the output folder.
-- Returns 'Nothing' if the file could not be read or the model did not parse.
processFile :: FilePath -> FilePath -> IO (Maybe Solution)
processFile inputFile outputFile = do
    putStrLn $ "Starting " ++ inputFile
    result <- try (readFile inputFile) :: IO (Either IOException String)
    case result of
        Left e
            | isDoesNotExistError e -> do
                putStrLn $ "File not found: " ++ inputFile
                pure Nothing
            | otherwise -> do
                putStrLn $ "Error reading file " ++ inputFile ++ ": " ++ show e
                pure Nothing
        Right contents -> do
            case parse miniZincParser contents of
                Result _ adt -> do
                    let solution = solveProblem adt
                        solved = maybe "Nothing" show solution
                    writeFile outputFile (solved ++ "\n")
                    putStrLn $ "Processed: " ++ inputFile
                    pure (Just solution)
                _ -> do
                    putStrLn $ "Invalid ParseResult for file " ++ inputFile
                    pure Nothing

-- | Check one input's result against all of its accepted answers.
checkInput :: FilePath -> IO Bool
checkInput inputFile = do
    let actualOutputFile = genOutputName "output" "out" inputFile
    actual <- processFile inputFile actualOutputFile
    expectedLines <- lines <$> readFile (expectedPath inputFile)
    let ok = case actual of
            Nothing -> False
            Just solution -> isAccepted expectedLines solution
    putStrLn $
        "Checking "
            ++ inputFile
            ++ " against accepted solutions: "
            ++ (if ok then "PASS" else "FAIL")
    pure ok

-- | Check that a valid model produces an expected solution.
checkSolution
    :: String
    -> String
    -> (Solution -> Bool)
    -> IO Bool
checkSolution testName source expected = do
    let result =
            case parse miniZincParser source of
                Error _ -> Nothing
                Result _ ast -> Just (solveProblem ast)

    let passed =
            case result of
                Nothing -> False
                Just solution -> expected solution

    putStrLn $
        (if passed then "PASS: " else "FAIL: ")
            ++ testName

    pure passed

-- Check that invalid MiniZinc input is rejected.
checkInvalid :: String -> String -> IO Bool
checkInvalid testName source = do
    let passed =
            case parse miniZincParser source of
                Error _ -> True
                Result _ _ -> False

    putStrLn $
        (if passed then "PASS: " else "FAIL: ")
            ++ testName

    pure passed

-- Regression tests covering the main language features.
runRegressionTests :: IO Bool
runRegressionTests = do
    results <- sequence
        [ checkSolution
            "Signed integer range"
            "var -3..3: x; constraint x = -2; solve satisfy;"
            (== Just [("x", -2)])

        , checkSolution
            "Finite-set domain"
            "var {1,3,5}: x; constraint x > 3; solve satisfy;"
            (== Just [("x", 5)])

        , checkSolution
            "Constant-based domain"
            "int: n = 3; var 1..n: x; solve maximize x;"
            (== Just [("x", 3)])

        , checkSolution
            "Arithmetic precedence"
            "var 1..20: x; constraint x = 2 + 3 * 4; solve satisfy;"
            (== Just [("x", 14)])

        , checkSolution
            "Right-associative exponentiation"
            "var 1..600: x; constraint x = 2^3^2; solve satisfy;"
            (== Just [("x", 512)])

        , checkSolution
            "Numeric sum macro"
            "var 1..5: x; constraint sum([x,2,3]) = 7; solve satisfy;"
            (== Just [("x", 2)])

        , checkSolution
            "Boolean all macro"
            "var 1..5: x; constraint all([x>2,x<4]); solve satisfy;"
            (== Just [("x", 3)])

        , checkSolution
            "Optimization"
            "var 1..5: x; constraint x <= 4; solve maximize x;"
            (== Just [("x", 4)])

        , checkSolution
            "Unsatisfiable model"
            "var 1..3: x; constraint x > 10; solve satisfy;"
            (== Nothing)

        , checkInvalid
            "Duplicate identifier"
            "var 1..3: x; var 1..5: x; solve satisfy;"

        , checkInvalid
            "Undefined identifier"
            "var 1..3: x; constraint x = y; solve satisfy;"

        , checkInvalid
            "Invalid domain"
            "var 5..1: x; solve satisfy;"
                , checkSolution
            "Array indexing"
            ( "array[1..2] of var 1..3: q; "
                ++ "constraint q[1] = 2; "
                ++ "constraint q[2] = 3; "
                ++ "solve satisfy;"
            )
            (== Just [("q[1]", 2), ("q[2]", 3)])

        , checkSolution
            "Array allDifferent"
            ( "array[1..2] of var 1..2: q; "
                ++ "constraint allDifferent(q); "
                ++ "solve satisfy;"
            )
            (== Just [("q[1]", 1), ("q[2]", 2)])

        , checkSolution
            "Generator sum"
            ( "constraint sum(i in 1..3)(i*2) = 12; "
                ++ "solve satisfy;"
            )
            (== Just [])

        , checkSolution
            "Four Queens"
            ( "int: n = 4; "
                ++ "array[1..n] of var 1..n: q; "
                ++ "constraint allDifferent(q); "
                ++ "constraint allDifferent(i in 1..n)(q[i]+i); "
                ++ "constraint allDifferent(i in 1..n)(q[i]-i); "
                ++ "solve satisfy;"
            )
            (\solution ->
                normalise solution `elem`
                    [ Just
                        [ ("q[1]", 2)
                        , ("q[2]", 4)
                        , ("q[3]", 1)
                        , ("q[4]", 3)
                        ]
                    , Just
                        [ ("q[1]", 3)
                        , ("q[2]", 1)
                        , ("q[3]", 4)
                        , ("q[4]", 2)
                        ]
                    ])]

    pure (and results)

main :: IO ()
main = do
    putStrLn "Testing "
    checkADTExampleModel
    checkSolverExampleModel

    putStrLn ""
    putStrLn "Testing example inputs:"

    inputFiles <- glob "examples/inputs/*.mzn"

    mkFolderIfNotExists "./examples/output"

    results <- forM inputFiles checkInput

    putStrLn ""
    putStrLn $
        "Matched an accepted solution for "
            ++ show (length (filter id results))
            ++ " of "
            ++ show (length results)
            ++ " example input(s)."

    putStrLn "\nRunning additional regression tests:"
    
    regressionPassed <- runRegressionTests

    putStrLn "\nAll processing complete."

    unless (and results && regressionPassed) exitFailure

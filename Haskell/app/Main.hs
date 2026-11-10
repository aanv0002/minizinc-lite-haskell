{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing and
-- solving MiniZinc-lite models. This server exposes an API that accepts a
-- model, parses and solves it, and returns the resulting assignment.
module Main (main) where

{- |
AI Assistance: ChatGPT
Assisted with the Save API, solution formatting, and file generation.
-}

import Assignment (getTime, miniZincParser, solveProblem)
import Control.Arrow (second)
import Control.Concurrent.QSemN (newQSemN)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Instances (ParseResult (..), parse)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- Convert a list of key-value pairs into JSON.
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (pack value :: Text)
            | (key, value) <- pairs
            ]

-- Parse and solve a MiniZinc model.
-- Return an error message if parsing or solving fails.
solveModel :: Text -> Either String [(String, Int)]
solveModel modelText =
    case parse miniZincParser (unpack modelText) of
        Error err ->
            Left (show err)

        Result _ model ->
            case solveProblem model of
                Nothing -> Left "No solution"
                Just solution -> Right solution

-- Format one variable assignment for an output file.
formatBinding :: (String, Int) -> String
formatBinding (name, value) =
    name ++ " = " ++ show value ++ ";"

-- Save a valid solution to a timestamped output file.
saveSolution :: [(String, Int)] -> IO FilePath
saveSolution solution = do
    timestamp <- getTime

    let filename = "solution_" ++ timestamp ++ ".out"

    let contents =
            unlines $
                ("-- Generated: " ++ timestamp)
                    : map formatBinding solution

    writeFile filename contents
    pure filename

-- Launch the HTTP server and register API routes.
main :: IO ()
main =
    newQSemN 1 >>= \_ -> scotty 3000 $ do

        -- Run a MiniZinc model and return the solution.
        post "/api/minizinc" $ do
            modelText <- formParam "model" :: ActionM Text

            case solveModel modelText of
                Left err ->
                    jsonResponse [("error", err)]

                Right solution ->
                    jsonResponse (map (second show) solution)

        -- Solve a model and save its solution to a file.
        post "/api/save" $ do
            modelText <- formParam "model" :: ActionM Text

            case solveModel modelText of
                Left err ->
                    jsonResponse [("error", err)]

                Right solution -> do
                    filename <- liftIO (saveSolution solution)

                    jsonResponse
                        [("result", "Saved to " ++ filename)]
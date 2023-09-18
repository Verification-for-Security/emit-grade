module Grade
  ( pretty
  , autograde
  ) where

import System.Environment (lookupEnv)
import System.IO (stdout)
import System.Console.ANSI
import Text.Printf

import Control.Monad (when)
import Data.Maybe (isJust)

-- | Emits the grade in a human readable format.
pretty :: Float -> IO ()
pretty grade = do
  let color = if grade >= 0.545 then Green else Red
  ansi <- hSupportsANSI stdout
  let setSGR' = when ansi . setSGR
  setSGR' [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR' [SetColor Foreground Vivid color]
  putStr . printf "%.1f" $ grade * 10
  setSGR' [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "/10.0]"
  setSGR' [Reset]

-- | Emits a grade compatible with the autograding environment.
autograde :: Float -> IO ()
autograde grade = do
  -- Checks whether we are in a codegrade environment.
  env <- lookupEnv "CG_INFO"
  when (isJust env) $ print grade

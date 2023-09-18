module Grade
  ( dutch
  , codegrade
  ) where

import System.Environment (lookupEnv)
import System.IO (stdout)
import System.Console.ANSI
import Text.Printf

import Control.Monad (when)
import Data.Maybe (isJust)

-- | Emits a grade according to the dutch grading system.
dutch :: Float -> IO ()
dutch grade = do
  let color = if grade > 0.55 then Green else Red
  ansi <- hSupportsANSI stdout
  let setSGR' = when ansi . setSGR
  setSGR' [SetConsoleIntensity BoldIntensity]
  putStr "Your current grade is: ["
  setSGR' [SetColor Foreground Vivid color]
  putStr . printf "%.1f" $ grade * 10
  setSGR' [Reset, SetConsoleIntensity BoldIntensity]
  putStrLn "/10.0]"
  setSGR' [Reset]

-- | Emits a codegrade parsable grade when inside of
-- a codegrade environemnt.
codegrade :: Float -> IO ()
codegrade grade = do
  env <- lookupEnv "CG_INFO"
  when (isJust env) $ print grade

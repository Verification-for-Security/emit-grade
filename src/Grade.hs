module Grade
  ( pretty
  , autograde
  ) where

import System.Environment (lookupEnv)
import System.IO (stdout)
import System.Console.ANSI
import Text.Printf

import Control.Monad.IO.Class
import Control.Monad (when)
import Data.Maybe (isJust)

-- | Emits the grade in a human readable format.
pretty :: MonadIO m => Float -> m ()
pretty grade = do
  let color = if grade >= 0.545 then Green else Red
  ansi <- liftIO $ hSupportsANSI stdout
  let setSGR' = when ansi . liftIO . setSGR
  setSGR' [SetConsoleIntensity BoldIntensity]
  liftIO $ putStr "Your current grade is: ["
  setSGR' [SetColor Foreground Vivid color]
  liftIO . putStr . printf "%.1f" $ grade * 10
  setSGR' [Reset, SetConsoleIntensity BoldIntensity]
  liftIO $ putStrLn "/10.0]"
  setSGR' [Reset]

-- | Emits a grade compatible with the autograding environment.
autograde :: MonadIO m => Float -> m ()
autograde grade = do
  -- Checks whether we are in a codegrade environment.
  env <- liftIO $ lookupEnv "CG_INFO"
  when (isJust env) . liftIO . print $ grade

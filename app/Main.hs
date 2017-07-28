module Main where

import Golf
import Language.Haskell.Interpreter


demort :: String -> String -> IO ()
demort cmd ('(':stresult) = demort cmd (init stresult)
demort cmd stresult = putStrLn (cmd ++ " = " ++ stresult)

demor :: String -> String -> IO ()
demor cmd sresult = demort cmd (drop 6 sresult) 

imports = ["Prelude","Golf"]

demoStrs :: String -> IO ()
demoStrs cmd = do
  result <- runInterpreter $ setImports imports >> interpret cmd (as :: [String])
  demor cmd (show result)

main :: IO ()
main = do
  putStrLn "CIS194 Homework 3"

  putStrLn "\nExercise 1"
  demoStrs "skips \"ABCD\""
  demoStrs "skips \"hello!\""

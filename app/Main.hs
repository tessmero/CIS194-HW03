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

demoInts :: String -> IO ()
demoInts cmd = do
  result <- runInterpreter $ setImports imports >> interpret cmd (as :: [[Int]])
  demor cmd (show result)

demoBuls :: String -> IO ()
demoBuls cmd = do
  result <- runInterpreter $ setImports imports >> interpret cmd (as :: [[Bool]])
  demor cmd (show result)

demoIntl :: String -> IO ()
demoIntl cmd = do
  result <- runInterpreter $ setImports imports >> interpret cmd (as :: [Integer])
  demor cmd (show result)

main :: IO ()
main = do
  putStrLn "CIS194 Homework 3"

  putStrLn "\nExercise 1"
  demoStrs "skips \"ABCD\"                      "
  demoStrs "skips \"hello!\"                    "
  demoInts "skips [1]                         "
  demoBuls "skips [True,False]                "

  putStrLn "\nExercise 2"
  demoIntl "localMaxima [2,9,5,6,1]           "
  demoIntl "localMaxima [2,3,4,1,5]           "
  demoIntl "localMaxima [1,2,3,4,5]           "

  putStrLn "\nExercise 3"
  putStrLn "histogram [1,1,1,5]                =\n"
  putStrLn (histogram [1,1,1,5])
  putStrLn "histogram [1,4,5,4,6,6,3,4,2,4,9]  =\n"
  putStrLn (histogram [1,4,5,4,6,6,3,4,2,4,9])

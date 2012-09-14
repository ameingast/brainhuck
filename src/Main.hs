module Main (
  main
) where

import Brainhuck.Interpreter(execute)

import System.Environment(getArgs)

main :: IO ()
main = getArgs >>= \args -> case args of
  "-h":_ -> putStrLn "brainhuck [-hi] <files>"
  "-i":_ -> getLine >>= execute >> putStrLn ""
  files -> mapM_ (\f -> readFile f >>= execute) files >> putStrLn ""
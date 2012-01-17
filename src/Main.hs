module Main (
  main
) where

import Brainhuck.Interpreter(execute)

import System(getArgs)

main :: IO ()
main = getArgs >>= \args -> case args of
  "-h":_ -> putStrLn "brainhuck [-h] <files>"
  files -> mapM_ (\f -> readFile f >>= execute) files >> putStrLn ""
module Brainhuck.Interpreter (
  execute
) where

import Brainhuck.State
import Brainhuck.Util.List(replace)

import Control.Monad(liftM)
import Data.Char(digitToInt)

interpret :: State -> IO (State)
interpret s@(xs, i, p, t)
  | i == length xs = return nullState
  | c == '>' = interpret (xs, i + 1, p + 1, t)
  | c == '<' = interpret (xs, i + 1, p - 1, t)
  | c == '+' = interpret (xs, i + 1, p, replace t p (+1))
  | c == '-' = interpret (xs, i + 1, p, replace t p (+ (-1)))
  | c == '.' = interpretPrint s
  | c == ',' = interpretRead s
  | c == ']' = interpretUnloop s
  | c == '[' = interpretLoop s
  | otherwise = (interpret . advance) s
    where c = xs !! i

interpretPrint :: State -> IO (State)
interpretPrint s@(_, _, p, t) = putStr c >> (interpret . advance) s
  where c = show $ t !! p

interpretRead :: State -> IO (State)
interpretRead (xs, i, p, t) = do
  d <- liftM digitToInt getChar
  interpret (xs, i + 1, p , replace t p (\_ -> d))

interpretLoop :: State -> IO (State)
interpretLoop s@(_, _, p, t)
  | d > 0 = (interpret . advance) s
  | otherwise = (interpret . seekForward 1 . advance) s
    where d = t !! p

interpretUnloop :: State -> IO (State)
interpretUnloop = interpret . seekBack 1 . regress

seekForward :: Int -> State -> State
seekForward l s@(xs, i, _, _)
  | l == 0 = s
  | c == '[' = seekForward (l + 1) s'
  | c == ']' = seekForward (l - 1) s'
  | otherwise = seekForward l s'
    where
      c = xs !! i
      s' = advance s

seekBack :: Int -> State -> State
seekBack l s@(xs, i, _, _)
  | l == 0 = s'
  | c == ']' = seekBack (l + 1) s''
  | c == '[' = seekBack (l - 1) s''
  | otherwise = seekBack l s''
    where
      c = xs !! i
      s' = advance s
      s'' = regress s

execute :: String -> IO (State)
execute = interpret . makeState

module Brainhuck.State where

type Tape = [Int]
type Pointer = Int
type Input = String
type State = (Input, Pointer, Pointer, Tape)

makeState :: String -> State
makeState s = (s, 0, 0, iterate id 0)

nullState :: State
nullState = ([], 0, 0, [])

advance :: State -> State
advance (xs, i, p, t) = (xs, i + 1, p, t)

regress :: State -> State
regress (xs, i, p, t) = (xs, i - 1, p, t)
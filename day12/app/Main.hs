module Main where

import Lib

main :: IO ()
main = do
    let initialState = [ moon [ -9, 10, -1]
                       , moon [-14, -8, 14]
                       , moon [  1,  5,  6]
                       , moon [-19,  7,  8]
                       ]
    let simulation = simulate initialState
    let finalState = simulation !! 1000
    let systemTotalEnergy = sum $ map totalEnergy finalState
    print systemTotalEnergy
    print $ cycleLength' simulation

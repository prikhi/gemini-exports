module Main where

import           Console.Gemini.Exports.Main


main :: IO ()
main = getArgs >>= run

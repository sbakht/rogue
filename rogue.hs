module Main where

main :: IO ()
main = do
  loop

loop :: IO ()
loop = do
  input <- getLine
  putStrLn $ "Rogue code here - " ++ start
  loop

start :: String
start = "0"
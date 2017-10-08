module Main where

main :: IO ()
main = do
  input <- getLine
  putStrLn $ "Rogue code here - " ++  input
  main
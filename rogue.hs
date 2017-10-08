module Main where

main :: IO ()
main = do
  loop []

loop :: [String] -> IO ()
loop start = do
  input <- getLine
  let accum = start ++ [input]
  putStrLn $ "Rogue code here - " ++ show accum
  loop accum

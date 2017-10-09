module Main where


data Cord = Cord (Int, Int)

data Input =
    IUp
  | IDown
  | ILeft
  | IRight 
    deriving (Show)

isValid :: String -> Bool
isValid = undefined

getInput :: IO (String)
getInput = getLine

runInput :: [Input] -> String -> [Input]
runInput inputs s =
    case toInput s of
        (Just x) -> x : inputs
        Nothing  -> inputs

toInput :: String -> Maybe Input
toInput "1" = Just ILeft
toInput "2" = Just IUp
toInput "3" = Just IRight
toInput "4" = Just IDown
toInput _ = Nothing

main :: IO ()
main = do
  loop []

loop :: [Input] -> IO ()
loop prevInputs = do
  input <- getInput
  let inputs = runInput prevInputs input
  putStrLn $ "Rogue code here - " ++ show inputs
  loop inputs

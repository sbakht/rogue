module Main where


data Cord = Cord (Int, Int) deriving (Eq, Show)

data Input =
    IUp
  | IDown
  | ILeft
  | IRight 
    deriving (Show)

data Player = Player { cord :: Cord } deriving (Show)

isValid :: String -> Bool
isValid = undefined

getInput :: IO (Maybe Input)
getInput = do 
    input <- getChar
    return $ toInput input

runInput :: [Input] -> Maybe Input -> [Input]
runInput inputs (Just x) = x : inputs
runInput inputs Nothing = inputs

toInput :: Char -> Maybe Input
toInput 'a' = Just ILeft
toInput 'w' = Just IUp
toInput 'd' = Just IRight
toInput 's' = Just IDown
toInput _ = Nothing

movePlayer :: Player -> Maybe Input -> Player
movePlayer (Player cord) (Just input) 
    | (isWallLoc newPos) = Player cord
    | otherwise          = Player newPos 
    where newPos = moveObject cord input
movePlayer player Nothing = player

moveObject :: Cord -> Input -> Cord
moveObject (Cord (x,y)) IUp = Cord (x, y - 1)
moveObject (Cord (x,y)) IDown = Cord (x, y + 1)
moveObject (Cord (x,y)) IRight = Cord (x + 1, y)
moveObject (Cord (x,y)) ILeft = Cord (x - 1, y)

playerStart :: Player
playerStart = Player (Cord (0,0))

isPlayerLoc :: Cord -> Cord -> Bool
isPlayerLoc = (==)

walls :: [Cord]
walls = fmap Cord [(1,1), (2,1)]

isWallLoc :: Cord -> Bool
isWallLoc cord = elem cord walls

showWorld :: Player -> IO ()
showWorld player = putStrLn $ buildWorld (cord player) 3 3 

buildWorld :: Cord -> Int -> Int -> String
buildWorld cord n n' = go 0 0 
    where go x y 
            | (y == n') = [] 
            | (x == n)  = ['\n'] ++ go 0 (y + 1)
            | isPlayerLoc cord (Cord (x,y)) = ['@'] ++ go (x + 1) y
            | isWallLoc (Cord (x,y)) = ['#'] ++ go (x + 1) y
            | otherwise = ['-'] ++ go (x + 1) y


main :: IO ()
main = do
  loop [] (Player (Cord (0,0)))

loop :: [Input] -> Player -> IO ()
loop prevInputs player = do
  input <- getInput
  let inputs = runInput prevInputs input
  let player' = movePlayer player input
  putStrLn $ "Rogue code here - " ++ show inputs
  showWorld player'
  print player'
  loop inputs player'

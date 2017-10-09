module Main where


data Cord = Cord (Int, Int) deriving (Eq, Show)

data Input =
    IUp
  | IDown
  | ILeft
  | IRight 
    deriving (Show)

data Player = Player { wCord :: Cord } deriving (Show)

data World = World { wPlayer :: Player
                    ,wWalls :: [Cord] }

isValidMove :: World -> Maybe Input -> Bool
isValidMove world (Just input)
    | isWallLoc world newPos = False
    | otherwise = True
    where newPos = moveObject (playerCord world) input
isValidMove _ Nothing = False

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

movePlayer :: World -> Maybe Input -> Player
movePlayer world (Just input) = Player $ moveObject (playerCord world) input
movePlayer world Nothing = wPlayer world

moveObject :: Cord -> Input -> Cord
moveObject (Cord (x,y)) IUp = Cord (x, y - 1)
moveObject (Cord (x,y)) IDown = Cord (x, y + 1)
moveObject (Cord (x,y)) IRight = Cord (x + 1, y)
moveObject (Cord (x,y)) ILeft = Cord (x - 1, y)

playerCord :: World -> Cord
playerCord = wCord . wPlayer

isPlayerLoc :: World -> Cord -> Bool
isPlayerLoc world = (==) (playerCord world)

walls :: [Cord]
walls = fmap Cord [(1,1), (2,1)]

isWallLoc :: World -> Cord -> Bool
isWallLoc world cord = elem cord (wWalls world)

showWorld :: World -> IO ()
showWorld world = putStrLn $ buildWorld world 3 3 

buildWorld :: World -> Int -> Int -> String
buildWorld world n n' = go 0 0 
    where go x y 
            | (y == n') = [] 
            | (x == n)  = ['\n'] ++ go 0 (y + 1)
            | isPlayerLoc world (Cord (x,y)) = ['@'] ++ go (x + 1) y
            | isWallLoc world (Cord (x,y)) = ['#'] ++ go (x + 1) y
            | otherwise = ['-'] ++ go (x + 1) y


main :: IO ()
main = do
  loop [] (World (Player (Cord (0,0))) walls)

loop :: [Input] -> World -> IO ()
loop prevInputs world = do
  showWorld world
  input <- getInput
  let inputs = runInput prevInputs input
  let player' = if isValidMove world input then movePlayer world input else wPlayer world
  let world' = World player' walls 
  putStrLn $ "Rogue code here - " ++ show inputs
  print player'
  loop inputs world'

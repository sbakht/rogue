module Main where

import Data.List (sort)

data Cord = Cord (Int, Int) deriving (Ord, Eq, Show)

data Input =
    IUp
  | IDown
  | ILeft
  | IRight 
    deriving (Show)

data World = World { wPlayer :: Cord
                    ,wWalls :: [Cord]
                    ,wCrates :: [Cord]
                    ,wStorage :: [Cord]
                    ,wWidth :: Int
                    ,wHeight :: Int }


---------------------------------------------------------------------------

isPlayer :: World -> Cord -> Bool
isPlayer world = (==) (wPlayer world)

isWall :: World -> Cord -> Bool
isWall world cord = elem cord (wWalls world)

isCrate :: World -> Cord -> Bool
isCrate world cord = elem cord (wCrates world)

isStorage :: World -> Cord -> Bool
isStorage world cord = elem cord (wStorage world)

hasWon :: World -> Bool
hasWon world = sort (wStorage world) == sort (wCrates world)

isValidMove :: World -> Maybe Input -> Bool
isValidMove world (Just input)
    | isWall world newPos = False
    | isCrate world newPos = not (isWall world newCratePos || isCrate world newCratePos)
    | otherwise = True
    where newPos = moveObject (wPlayer world) input
          newCratePos = moveObject (newPos) input
isValidMove _ Nothing = False

---------------------------------------------------------------------------

toInput :: Char -> Maybe Input
toInput 'a' = Just ILeft
toInput 'w' = Just IUp
toInput 'd' = Just IRight
toInput 's' = Just IDown
toInput _ = Nothing

runInput :: [Input] -> Maybe Input -> [Input]
runInput inputs (Just x) = x : inputs
runInput inputs Nothing = inputs

---------------------------------------------------------------------------

moveObject :: Cord -> Input -> Cord
moveObject (Cord (x,y)) IUp = Cord (x, y - 1)
moveObject (Cord (x,y)) IDown = Cord (x, y + 1)
moveObject (Cord (x,y)) IRight = Cord (x + 1, y)
moveObject (Cord (x,y)) ILeft = Cord (x - 1, y)

playerCord :: [(Char, (Int, Int))] -> Cord
playerCord = head . cordsOf '@'

wallCords :: [(Char, (Int, Int))] -> [Cord]
wallCords = cordsOf '#'

crateCords :: [(Char, (Int, Int))] -> [Cord]
crateCords = cordsOf 'o'

storageCords :: [(Char, (Int, Int))] -> [Cord]
storageCords = cordsOf '.'

cordsOf :: Char -> [(Char, (Int, Int))] -> [Cord]
cordsOf c = map (Cord . snd) . filter ((== c) . fst)

---------------------------------------------------------------------------

level :: [String]
level = ["#####"
       ,"#.o@#"
       ,"#---#"
       ,"#####"]

instance Show World where
  show world = go 0 0 
    where go x y 
            | (y == height) = [] 
            | (x == width)  = ['\n'] ++ go 0 (y + 1)
            | isPlayer world (Cord (x,y)) = ['@'] ++ go (x + 1) y
            | isWall world (Cord (x,y)) = ['#'] ++ go (x + 1) y
            | isCrate world (Cord (x,y)) = ['o'] ++ go (x + 1) y
            | isStorage world (Cord (x,y)) = ['.'] ++ go (x + 1) y
            | otherwise = [' '] ++ go (x + 1) y
          height = wHeight world
          width = wWidth world

---------------------------------------------------------------------------

levelToCords :: [String] -> [(Char, (Int, Int))]
levelToCords level = concat $ zipWith (\x y -> zipWith (\x z -> (x,(z,y)) ) x [0..]) level [0..]

triggerMove :: World -> Maybe Input -> World
triggerMove world (Just input) = world {wPlayer = newPos, wCrates = crates'}
    where newPos = moveObject (wPlayer world) input
          crates = filter ((/=) newPos) (wCrates world)
          isNextCrate = length (filter ((==) newPos) (wCrates world)) == 1
          crates' = if isNextCrate
            then (moveObject newPos input) : crates
            else crates

          newCratePos = moveObject (newPos) input
triggerMove world Nothing = world

loadLevel :: [String] -> World
loadLevel xs = (World 
                <$> playerCord <*> wallCords <*> crateCords <*> storageCords $ cords) 
                <$> width <*> height $ xs
  where cords = levelToCords xs
        width = maximum . map length
        height = length 

getInput :: IO (Maybe Input)
getInput = do 
    input <- getChar
    return (toInput input)

main :: IO ()
main = do
  let world = loadLevel level
  print world
  loop [] world

loop :: [Input] -> World -> IO ()
loop prevInputs world = do
  input <- getInput
  let inputs = runInput prevInputs input
  let world' = if isValidMove world input 
                then triggerMove world input 
                else world
  print world'
  if hasWon world'
    then putStrLn "You Won!" 
    else loop inputs world'

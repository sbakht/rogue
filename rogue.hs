module Main where

import Data.List (sort)

type Player = Cord
type Wall = Cord
type Crate = Cord
type Storage = Cord

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

isCratePushable :: World -> Crate -> Input -> Bool
isCratePushable world cord input = not (isWall world newPos || isCrate world newPos)
    where newPos :: Crate
          newPos = moveObject cord input

isValidMove :: World -> Maybe Input -> Bool
isValidMove world (Just input)
    | isWall world newPos = False
    | isCrate world newPos = isCratePushable world newPos input
    | otherwise = True
    where newPos :: Player
          newPos = moveObject (wPlayer world) input
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

playerCord :: [(Char, Cord)] -> Player
playerCord = head . cordsOf '@'

wallCords :: [(Char, Cord)] -> [Wall]
wallCords = cordsOf '#'

crateCords :: [(Char, Cord)] -> [Crate]
crateCords = cordsOf 'o'

storageCords :: [(Char, Cord)] -> [Storage]
storageCords = cordsOf '.'

cordsOf :: Char -> [(Char, Cord)] -> [Cord]
cordsOf c = map snd . filter ((== c) . fst)

allCords :: Int -> Int -> [[Cord]]
allCords width height = map (\y -> map (\x -> Cord (x,y)) [0..(width - 1)]) [0..(height - 1)]  

levelToCords :: [String] -> [(Char, Cord)]
levelToCords level = concat $ zipWith zip level (allCords width height)
  where width = maximum . map length $ level
        height = length level

---------------------------------------------------------------------------

level :: [String]
level = ["#####"
       ,"#.o@#"
       ,"#---#"
       ,"#####"]

instance Show World where
  show world = unlines . (map . map) toChar $ cords
    where cords = allCords (wWidth world) (wHeight world)
          toChar cord 
            | isPlayer world cord = '@'
            | isWall world cord = '#'
            | isCrate world cord = 'o'
            | isStorage world cord = '.'
            | otherwise = ' '

---------------------------------------------------------------------------

updateWorld :: World -> Maybe Input -> World
updateWorld world (Just input) = world {wPlayer = newPos, wCrates = crates'}
    where newPos = moveObject (wPlayer world) input
          crates = filter ((/=) newPos) (wCrates world)
          isNextCrate = length (filter ((==) newPos) (wCrates world)) == 1
          crates' = if isNextCrate
            then (moveObject newPos input) : crates
            else crates
updateWorld world Nothing = world

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
                then updateWorld world input 
                else world
  print world'
  if hasWon world'
    then putStrLn "You Won!" 
    else loop inputs world'

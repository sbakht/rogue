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

playerCord :: [(Char, (Int, Int))] -> Player
playerCord = head . cordsOf '@'

wallCords :: [(Char, (Int, Int))] -> [Wall]
wallCords = cordsOf '#'

crateCords :: [(Char, (Int, Int))] -> [Crate]
crateCords = cordsOf 'o'

storageCords :: [(Char, (Int, Int))] -> [Storage]
storageCords = cordsOf '.'

cordsOf :: Char -> [(Char, (Int, Int))] -> [Cord]
cordsOf c = map (Cord . snd) . filter ((== c) . fst)

---------------------------------------------------------------------------

level :: [String]
level = ["#####"
       ,"#.o@#"
       ,"#---#"
       ,"#####"]

allCords :: World -> [[Cord]]
allCords world = map (\y -> map (\x -> Cord (x,y)) [0..(wWidth world - 1)]) [0..(wHeight world - 1)]  

instance Show World where
  show world = unlines $ (map . map) go (allCords world) 
    where go cord 
            | isPlayer world cord = '@'
            | isWall world cord = '#'
            | isCrate world cord = 'o'
            | isStorage world cord = '.'
            | otherwise = ' '

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

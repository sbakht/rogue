module Main where


data Cord = Cord (Int, Int) deriving (Eq, Show)

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

movePlayer :: World -> Maybe Input -> Cord
movePlayer world (Just input) = moveObject (wPlayer world) input
movePlayer world Nothing = wPlayer world

moveObject :: Cord -> Input -> Cord
moveObject (Cord (x,y)) IUp = Cord (x, y - 1)
moveObject (Cord (x,y)) IDown = Cord (x, y + 1)
moveObject (Cord (x,y)) IRight = Cord (x + 1, y)
moveObject (Cord (x,y)) ILeft = Cord (x - 1, y)

playerCord :: [(Char, (Int, Int))] -> Cord
playerCord = Cord . snd . head . filter ((== '@') . fst)

wallCords :: [(Char, (Int, Int))] -> [Cord]
wallCords = map (Cord . snd) . filter ((== '#') . fst)

crateCords :: [(Char, (Int, Int))] -> [Cord]
crateCords = map (Cord . snd) . filter ((== 'o') . fst)

storageCords :: [(Char, (Int, Int))] -> [Cord]
storageCords = map (Cord . snd) . filter ((== '.') . fst)

---------------------------------------------------------------------------

level :: [String]
level = ["#####"
       ,"#.o@#"
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
            | otherwise = ['-'] ++ go (x + 1) y
          height = wHeight world
          width = wWidth world

---------------------------------------------------------------------------

levelCords :: [String] -> [(Char, (Int, Int))]
levelCords level = concat $ zipWith (\x y -> zipWith (\x z -> (x,(z,y)) ) x [0..]) level [0..]

loadLevel :: [String] -> World
loadLevel xs = World {wPlayer = player
                     ,wWalls = walls
                     ,wCrates = crates
                     ,wStorage = storage
                     ,wWidth = width 
                     ,wHeight = height}
  where cords = levelCords xs
        player = playerCord cords
        walls = wallCords cords
        crates = crateCords cords
        storage = storageCords cords
        width = maximum $ map length xs
        height = length xs 

getInput :: IO (Maybe Input)
getInput = do 
    input <- getChar
    return (toInput input)

main :: IO ()
main = do
  loop [] (loadLevel level)

loop :: [Input] -> World -> IO ()
loop prevInputs world = do
  print world
  input <- getInput
  let inputs = runInput prevInputs input
  let player' = if isValidMove world input 
                then movePlayer world input 
                else wPlayer world
  let world' = world {wPlayer = player'}
  print player'
  loop inputs world'

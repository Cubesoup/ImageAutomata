

module Main where

import Codec.Picture
import Data.Word
import Data.Convertible
import System.Random
import System.Environment    


-- WARNING: your stochastic 'run' functions do not work. The step function seems to work fine,
-- but subsequent steps do not appear to have different random values! Might have to do with lazy
-- evaluation. Design a test that can detect whether or not subsequent steps are actually being
-- executed, but have the same random values (maybe the pixel that (randomly) selected cells become
-- changes over time?)

-- update: indeed, the generator isn't being changed between steps. (multiple steps do in fact execute)

-- TODO: Add capability for randomness, Make hella fast. Better interface. More automata.
    
-- this is about where I'm at with the reimplementation:
main :: IO ()
main = do
  args <- getArgs
  let path = head args
  test path

test :: FilePath -> IO ()
test path = do
  img <- loadImage path
  gen <- newStdGen
  let frames = runStochastic gen 100 fightRule img
  putStrLn "constructing GIF, this may take a long time"
  animate frames "output.gif"
               
--------------------------------------------    
-- Data Types for Image Cellular Automata --
--------------------------------------------

-- This could encode a lot more structure, but for the sake of flexibility,
-- it doesn't. Note also that the "run" function generates an infinite list
-- of the generated images in order. The idea being that we take however many
-- we need, and don't bother to compute the rest. 
    
type Cell = PixelRGBA8

type Grid = Image PixelRGBA8
    
type Coordinate = (Int,Int)

type RandomNumber = Int

type DeterministicRule = Coordinate -> Grid -> Cell

type StochasticRule = Coordinate -> RandomNumber -> Grid -> Cell

stepDeterministic :: DeterministicRule -> Grid -> Grid
stepDeterministic f grid = generateImage (\x y -> f (x,y) grid) (imageWidth grid)
                                                                (imageHeight grid)

-- the first argument is the number of steps we want to run for.
runDeterministic :: Int -> DeterministicRule -> Grid -> [Grid]
runDeterministic n f grid = take n (runForever f grid)
  where
    runForever :: DeterministicRule -> Grid -> [Grid]
    runForever f grid = grid : (runForever f (stepDeterministic f grid))


stepStochastic :: (RandomGen g) => g -> StochasticRule -> Grid -> (g,Grid)
stepStochastic gen f grid = generateFoldImage (\gen x y -> (snd (next gen) , f (x,y) (fst (next gen)) grid))
                                                 gen (imageWidth grid) (imageHeight grid)


runStochastic :: (RandomGen g) => g -> Int -> StochasticRule -> Grid -> [Grid]
runStochastic gen n f grid = take n (runForeverS gen f grid)

runForeverS :: (RandomGen g) => g -> StochasticRule -> Grid -> [Grid]
runForeverS gen f grid = grid : runForeverS gen' f grid'
  where
    (gen', grid') = stepStochastic gen f grid
    


-- runforever gen f grid = grid : runforever gen1 f (stepS gen2 f grid)
-- = grid : (stepS gen2 f grid) : runforever gen11 (stepS gen12 f grid)
-- = grid : (stepS gen2 f grid) : (stepS gen12 f grid) : runforever gen111 (stepS gen112 f grid)
-- = ...

-- this is where we do the "torus-side-connection" thing
cellAt :: Coordinate -> Grid -> Cell
cellAt (x,y) img = pixelAt img (x `mod` (imageWidth img)) (y `mod` (imageHeight img))

--------------------------------
-- Reading and Writing Images --
--------------------------------             

loadImage :: FilePath -> IO Grid
loadImage path = do
  result <- readImage path 
  case result of
    (Left e) -> error $ "unable to load file " ++ path ++ " as an image, terminating"
    (Right dyn) -> return (convertRGBA8 dyn)

-- time to display each frame in the animation, in 100ths of a second.
frameTime :: Int
frameTime = 10       

-- animating a finite list of frames            
animate :: [Grid] -> FilePath -> IO ()
animate frames path = case writeGifAnimation path frameTime LoopingForever
                               (map (convertRGB8 . ImageRGBA8) frames)
                        of
                          (Left e) -> error $ "unable to write GIF animation to "
                                             ++ path ++ ", terminating"
                          (Right success) -> success >> putStrLn "Done!"

-- for doing that "play it forwards, then reverse to the start, forever" thing
invConcat :: [a] -> [a]
invConcat xs = xs ++ (reverse xs)             
    
-----------------
-- Pixel Logic --
-----------------

-- elementary pixel inspection and creation functions:

red :: PixelRGBA8 -> Word8
red (PixelRGBA8 x _ _ _) = x

green :: PixelRGBA8 -> Word8
green (PixelRGBA8 _ x _ _) = x
                           
blue :: PixelRGBA8 -> Word8
blue (PixelRGBA8 _ _ x _) = x

alpha :: PixelRGBA8 -> Word8
alpha (PixelRGBA8 _ _ _ x) = x

setRed :: Word8 -> PixelRGBA8 -> PixelRGBA8
setRed r (PixelRGBA8 _ g b a) = PixelRGBA8 r g b a

setGreen :: Word8 -> PixelRGBA8 -> PixelRGBA8
setGreen g (PixelRGBA8 r _ b a) = PixelRGBA8 r g b a

setBlue :: Word8 -> PixelRGBA8 -> PixelRGBA8
setBlue b (PixelRGBA8 r g _ a) = PixelRGBA8 r g b a

setAlpha :: Word8 -> PixelRGBA8 -> PixelRGBA8
setAlpha a (PixelRGBA8 r g b _) = PixelRGBA8 r g b a            

mostlyRed :: PixelRGBA8 -> Bool
mostlyRed p = ((red p) > (blue p)) && ((red p) > (green p))

mostlyBlue :: PixelRGBA8 -> Bool
mostlyBlue p = ((blue p) > (red p)) && ((blue p) > (green p))

mostlyGreen :: PixelRGBA8 -> Bool
mostlyGreen p = ((green p) > (red p)) && ((green p) > (blue p))                 
                                  
----------------
-- Rule Logic --
----------------

-- some common rule neighbourhoods

moore :: Coordinate -> Grid -> [Cell]
moore (x,y) img = map (\(x,y) -> cellAt (x,y) img)
                    [(x,y+1),(x,y-1),(x+1,y),(x-1,y)
                    ,(x+1,y+1),(x+1,y-1),(x-1,y-1),(x-1,y+1)]

-- the first four cells in the moore neighbourhood are the von-neumann neighbourhood (how convenient!)
vonNeumann :: Coordinate -> Grid -> [Cell]
vonNeumann img posn = take 4 (moore img posn)

-----------------------------------------                      
-- Simple Orthogonal Bleeding Automata --                      
-----------------------------------------

-- e.g. "most red" causes the reddest pixel to replace its less red neighbours
-- each iteration.

more :: (Cell -> Word8) -> Cell -> Cell -> Cell
more property a b = if property a > property b then a else b
                               
most :: (Cell -> Word8) -> DeterministicRule
most property posn grid = foldr1 (more property)
                               ((cellAt posn grid):(vonNeumann posn grid))

less :: (Cell -> Word8) -> Cell -> Cell -> Cell
less property a b = if property a > property b then b else a        
                                 
least :: (Cell -> Word8) -> DeterministicRule
least property posn grid = foldr1 (less property)
                                  ((cellAt posn grid):(vonNeumann posn grid))

-- what if we use the moore neighbourhood?
-- what if we don't include the cell itself in the list of possibles?
                                  
--------------------------------------------
-- Neighbour Component Averaging Automata --
--------------------------------------------

-- Colour values of cells become the average of their moore neighbourhood in
-- each component. This does what you expect, with the image slowly becoming
-- one colour, the original image becoming less and less distinct. creepy on faces.
-- All the variants here do more or less the same thing.

adoptMean :: DeterministicRule
adoptMean posn grid = (PixelRGBA8 (average (map red neighbours))
                                  (average (map green neighbours))
                                  (average (map blue neighbours))
                                  (average (map alpha neighbours)))
  where
    neighbours = moore posn grid                       

-- what if we include the cell itself when taking the mean?
adoptMean' :: DeterministicRule
adoptMean' posn grid = (PixelRGBA8 (average (map red neighbours))
                                   (average (map green neighbours))
                                   (average (map blue neighbours))
                                   (average (map alpha neighbours)))
  where
    neighbours = (cellAt posn grid) : (moore posn grid) -- (include self)

-- what if we take instead the von-neumann neighbourhood?
adoptOrthogonalMean :: DeterministicRule
adoptOrthogonalMean posn grid = (PixelRGBA8 (average (map red neighbours))
                                  (average (map green neighbours))
                                  (average (map blue neighbours))
                                  (average (map alpha neighbours)))
  where
    neighbours = vonNeumann posn grid -- what if include self? (why bother)?

average :: [Word8] -> Word8
average xs = convert (total `div` (length xs))
  where
    total :: Int
    total = sum (map convert xs)

-----------------------------------------------
-- Testing The Stochastic Automata Functions --
-----------------------------------------------

-- we define a CA where a cell cannot "see" its neighbours, and
-- simply has a random chance to turn a specific colour at each time step.

sometimesBecome :: PixelRGBA8 -> StochasticRule
sometimesBecome px (x,y) rand img = if (rand `mod` 10) == 0 then px
                                                            else cellAt (x,y) img

-- this one actually looks pretty cool, even when the random generation is flawed.
sometimesBrighter :: StochasticRule
sometimesBrighter (x,y) rand img = if (rand `mod` 10) == 0 then incremented
                                                           else cellAt (x,y) img
  where
    incremented :: PixelRGBA8
    incremented = incRed $ incBlue $ incGreen $ cellAt (x,y) img
    incRed :: PixelRGBA8 -> PixelRGBA8
    incRed px = setRed ((red px) + 10) px
    incBlue px = setBlue ((blue px) + 10) px
    incGreen px = setGreen ((green px) + 10) px

--------------------------------
-- The Colour Fight Automaton --
--------------------------------
-- Blue, green, and red dominated cells fight for control of the grid. This automaton is stochastic,
-- and uses the Moore neighbourhood

-- At each time step,
-- (if the cell is not black)
--  - if three or more enemy cells of the same colour are adjacent to a cell, it becomes dead (black)
--  - otherwise, if there are any adjacent enemy cells, the cell has a %50 chance of dying
-- (if the cell is black)
--  - if any non-black cells are adjacent, become one of them at random.

-- changed "black" to "max difference in colour < 50"
-- changed "%50 chance to die when enemy cells around" to "%20 chance"

fightRule :: StochasticRule
fightRule (x,y) n img = case army (cellAt (x,y) img) of
    Dead -> if aliveNeighbours == [] then (cellAt (x,y) img)
              else aliveNeighbours !! (n `mod` (length aliveNeighbours))
    _    -> if surrounded (cellAt (x,y) img) (moore (x,y) img) then (PixelRGBA8 0 0 0 255)
              else (if n `mod` 5 == 0 then (PixelRGBA8 0 0 0 255) else cellAt (x,y) img)          
  where
    neighbours :: [Cell]
    neighbours = moore (x,y) img
    aliveNeighbours :: [Cell]
    aliveNeighbours = filter (\c -> not (dead c)) neighbours

surrounded :: Cell -> [Cell] -> Bool
surrounded x nbh = case army x of
    Dead -> error "called partial function 'surrounded' on a Dead cell, which is undefined" -- False?
    Red -> (contains 3 Blue (map army nbh)) || (contains 3 Green (map army nbh))
    Blue -> (contains 3 Green (map army nbh)) || (contains 3 Red (map army nbh))
    Green -> (contains 3 Red (map army nbh)) || (contains 3 Blue (map army nbh))

contains :: (Eq a) => Int -> a -> [a] -> Bool
contains 0 _ _ = True
contains n y [] = False
contains n y (x:xs) = if x == y then contains (n-1) y xs else contains n y xs

-- let's say a cell is dead 
dead :: Cell -> Bool
dead px = (maximum diffs) < 50 
  where
    diffs :: [Int]
    diffs = map abs [(convert (red px)) - (convert (blue px))
                    ,(convert (red px)) - (convert (green px))
                    ,(convert (green px)) - (convert (blue px))]

data Army = Red | Blue | Green | Dead
  deriving (Eq)

-- note that red > green > blue in case of a tie.
army :: Cell -> Army
army px | dead px = Dead
        | (red px) > (green px) && (red px) > (blue px) = Red
        | (green px) > (blue px) = Green
        | otherwise = Blue

          
    
-----------            
-- Ideas --
-----------

-- look at CAs for algae growth
--         ... for disease spread
--         ... for predator prey modelling

-- take the cell above / below as neighbourhood, sort.

-- algal blooms may be caused by lack of algae-eating sea life. this suggests stages of modelling
-- stage one: have a colour range spread according to a simplified algal bloom spreading model
-- stage two: have a different colour range act like sea life to control the spread of the algae



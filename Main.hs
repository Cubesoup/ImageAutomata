module Main where

import Codec.Picture
import Data.Word
import Data.Convertible

import System.Environment    

-- TODO: Make hella fast. Better interface. More automata.
    
-- this is about where I'm at with the reimplementation:
main :: IO ()
main = do
  args <- getArgs
  let path = head args
  test path

test :: FilePath -> IO ()
test path = do
  img <- loadImage path
  let frames = run 300 adoptOrthogonalMean img
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

type Rule = Coordinate -> Grid -> Cell

step :: Rule -> Grid -> Grid
step f grid = generateImage (\x y -> f (x,y) grid) (imageWidth grid)
                                                   (imageHeight grid)

run :: Int -> Rule -> Grid -> [Grid]
run n f grid = take n (runForever f grid)

runForever :: Rule -> Grid -> [Grid]
runForever f grid = grid : (runForever f (step f grid))
                    
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
                               
most :: (Cell -> Word8) -> Rule
most property posn grid = foldr1 (more property)
                               ((cellAt posn grid):(vonNeumann posn grid))

less :: (Cell -> Word8) -> Cell -> Cell -> Cell
less property a b = if property a > property b then b else a        
                                 
least :: (Cell -> Word8) -> Rule
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

adoptMean :: Rule
adoptMean posn grid = (PixelRGBA8 (average (map red neighbours))
                                  (average (map green neighbours))
                                  (average (map blue neighbours))
                                  (average (map alpha neighbours)))
  where
    neighbours = moore posn grid                       

-- what if we include the cell itself when taking the mean?
adoptMean' :: Rule
adoptMean' posn grid = (PixelRGBA8 (average (map red neighbours))
                                   (average (map green neighbours))
                                   (average (map blue neighbours))
                                   (average (map alpha neighbours)))
  where
    neighbours = (cellAt posn grid) : (moore posn grid) -- (include self)

-- what if we take instead the von-neumann neighbourhood?
adoptOrthogonalMean :: Rule
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

-----------            
-- Ideas --
-----------

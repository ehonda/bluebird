module ConsoleDisplay where

import Control.Concurrent (threadDelay)
import Data.Colour.SRGB (sRGB24, sRGB)
import Data.Colour.RGBSpace
import Data.List.Split (chunksOf)
import Data.Natural

import System.Console.ANSI
import System.IO (hFlush, stdout)

-- Miscellaneous
-----------------------------------------------------

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: Int -> IO ()
pause nanos = hFlush stdout >> threadDelay nanos

-- Interpolation functions used for the color schemes
-----------------------------------------------------

interpolate :: Fractional a => Int -> a -> a -> [a]
interpolate 0 _ _ = []
interpolate steps start end =
    let delta = (end - start) / (fromIntegral steps)
    in map (\i -> start + delta * (fromIntegral i)) [0..steps]
    
interpTriple :: (Fractional a, Fractional b, Fractional c) => 
    Int -> (a, b, c) -> (a, b, c) -> [(a, b, c)]
interpTriple 0 _ _ = []
interpTriple steps (s_a, s_b, s_c) (e_a, e_b, e_c) =
    let i_a = interpolate steps s_a e_a
        i_b = interpolate steps s_b e_b
        i_c = interpolate steps s_c e_c
    in zipWith (\a (b, c) -> (a, b, c)) i_a $ zip i_b i_c
    
interpCircularUniform :: (Fractional a, Fractional b, Fractional c) =>
    Int -> [(a, b, c)] -> [(a, b, c)]
interpCircularUniform 0 _ = []
interpCircularUniform _ [] = []
interpCircularUniform steps points =
    let points' = points ++ [head points]
        startEnd = zip points' $ tail points'
        interOp = \(s, e) -> interpTriple steps s e
    in concat $ map (take steps) $ map interOp startEnd

-- COLOR SCHEMES
-----------------------------------------------------

-- Standard ANSI colors
-----------------------------------------
type ANSIColor = (Color, ColorIntensity)

vividANSI :: [ANSIColor]
vividANSI = zip (enumFromTo minBound maxBound) $ repeat Vivid

dullANSI :: [ANSIColor]
dullANSI = zip (enumFromTo minBound maxBound) $ repeat Dull

ansi16 :: [ANSIColor]
ansi16 = dullANSI ++ vividANSI

sgrANSI :: [ANSIColor] -> ConsoleLayer -> Natural -> SGR
sgrANSI colorInfo layer n =
    let index = (fromIntegral n) `mod` (length colorInfo)
        color = fst $ colorInfo !! index
        intensity = snd $ colorInfo !! index
    in SetColor layer intensity color
    
-- RGB colors
-----------------------------------------
-- RGB Cube Picture:
--  https://upload.wikimedia.org/wikipedia/commons/5/51/RGB_cube.jpg
rgbCubePath :: (Floating a, Ord a) => 
    Int -> [(a, a, a)] -> [Colour a]
rgbCubePath steps points = 
    let path = interpCircularUniform steps points
        op = \(a, b, c) -> sRGB a b c
    in map op path
    
rgbCubePathScaled :: (Floating a, Ord a) => 
    Int -> a -> [(a, a, a)] -> [Colour a]
rgbCubePathScaled steps s points =
    rgbCubePath steps $ map (\(a, b, c) -> (s*a, s*b, s*c)) points

rgbPath_rygcbm :: (Floating a, Ord a) => [(a, a, a)]
rgbPath_rygcbm = 
    [ (1, 0, 0)     -- Red
    , (1, 1, 0)     -- Yellow
    , (0, 1, 0)     -- Green
    , (0, 1, 1)     -- Cyan
    , (0, 0, 1)     -- Blue
    , (1, 0, 1)     -- Magenta
    ]
    
rgbPath_diags1 :: (Floating a, Ord a) => [(a, a, a)]
rgbPath_diags1 =
    [ (1, 0, 0)     -- Red
    , (0, 1, 1)     -- Cyan
    , (0, 1, 0)     -- Green
    , (1, 0, 1)     -- Magenta
    , (0, 0, 1)     -- Blue
    , (1, 1, 0)     -- Yellow
    ]

sgrRGB :: [Colour Float] -> ConsoleLayer -> Natural -> SGR
sgrRGB colors layer n =
    let index = (fromIntegral n) `mod` (length colors)
        color = colors !! index
    in SetRGBColor layer color

type ColorScheme = ConsoleLayer -> Natural -> SGR
    
sgrAllWhite :: ColorScheme
sgrAllWhite layer n = SetRGBColor layer $ sRGB 1 1 1

sgrAllBlack :: ColorScheme
sgrAllBlack layer n = SetRGBColor layer $ sRGB 0 0 0

sgrGray :: ColorScheme
sgrGray layer n = SetPaletteColor layer
    $ xterm24LevelGray $ (fromIntegral n) `mod` 24


-- Display Modes
-----------------------------------------
-- Prints a colored number
putNumber :: ColorScheme -> Natural -> IO ()
putNumber sgr n = setSGR [sgr Foreground n] >> putStr (show n)

-- Prints a colored number at least 2 characters wide
putNumber2 :: ColorScheme -> Natural -> IO ()
putNumber2 sgr n = do 
    setSGR [sgr Foreground n]
    if n < 10
    then putStr " " >> putStr (show n)
    else putStr (show n)

-- Prints a colored box 2 characters wide
putBox :: ColorScheme -> Natural -> IO ()
putBox sgr n = setSGR [sgr Background n] >> putStr "  "

-- Prints a colored box and a colored number at least 2
-- characters wide
putNumberBox :: ColorScheme -> ColorScheme -> Natural -> IO ()
putNumberBox fg bg n = do
    setSGR [fg Foreground n]
    setSGR [bg Background n]
    if n < 10
    then putStr " " >> putStr (show n)
    else putStr (show n)

type NatWord = [Natural]

-- Displays ws in the desired mode, scaled to size with the
-- passed delay in ns between printing rows
display :: (Natural -> IO ()) -> Int -> Int -> [NatWord] -> IO ()
display mode size delay ws = do
    let ws' = scale size $ map (scale size) ws
    mapM_ (\w -> mapM_ mode w >> putStrLn "" >> pause delay) ws'
    setSGR [Reset]
    where
        scale = \size w -> case w of
            [] -> []
            x : xs -> (replicate size x) ++ (scale size xs)

-- Displays ws in chunks of n, with a blank line between the chunks
displayChunksOf :: Int -> (Natural -> IO ()) -> [NatWord] -> IO ()
displayChunksOf n mode ws = do
    let chunks = chunksOf n ws
        chunkAction = mapM_ (\w -> mapM_ mode w >> putStrLn "")
    mapM_ (\ch -> chunkAction ch >> setSGR [Reset] >> putStrLn " ") chunks
    setSGR [Reset]
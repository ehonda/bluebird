module Main where

import Control.Monad
import Data.List
import Data.Natural
import Text.Printf

import ConsoleDisplay
import Bluebird

-- DEMO FUNCTIONS
-------------------------------------------
w_chunks9 = ((cycleSequences [2] w0_qlexDecr_dove) !! 2)
dove2_a = w_chunks9

rgbCompl2 = sgrRGB (rgbCubePathScaled 2 0.8 rgbPath_diags1)

coloredNumbers_0 = putNumber rgbCompl2
coloredBoxes_0 = putBox rgbCompl2
colNumBoxes_0 = putNumberBox sgrAllWhite rgbCompl2
colNumBoxes_1 = putNumberBox sgrAllBlack rgbCompl2

demo_color = display (putBox (sgrANSI vividANSI)) 1 0 $ ((cycleSequences [2] w0_qlexDecr_dove) !! 2)
demo_gray = display (putBox sgrGray) 1 0 $ ((cycleSequences [2] w0_qlexDecr_dove) !! 2)

demo_rgb = display (putBox (sgrRGB (rgbCubePathScaled 4 0.8 rgbPath_rygcbm))) 1 0 $ ((cycleSequences [2] w0_qlexDecr_dove) !! 2)
demo_rgb_diag = display (putBox (sgrRGB (rgbCubePathScaled 2 0.8 rgbPath_diags1))) 1 0 $ ((cycleSequences [2] w0_qlexDecr_dove) !! 2)

demo_chunks = displayChunksOf 9 coloredBoxes_0 $ w_chunks9
demo_chunk_dists = displayChunksOf 9 coloredBoxes_0 $ concat $ adjacentChunkDistances 9 w_chunks9

demo_numbered_chunks n w = displayChunksOf n colNumBoxes_0 $ concat $ adjacentChunkDistances n w
demo_boxes_chunks n w = displayChunksOf n coloredBoxes_0 $ concat $ adjacentChunkDistances n w


snippetColorScheme width height scheme = do
    let printRow = \mode -> display mode 1 0 [[0..(width - 1)]]
        printStripes = printRow $ putBox scheme
        printNumbers = printRow $ putNumber2 scheme
    replicateM_ height printStripes
    printNumbers

demo_color_scheme =
    let background = replicate 5 [0..50]
        colorSchemes =
            [ rgbCubePathScaled 1 0.8 rgbPath_rygcbm
            , rgbCubePathScaled 2 0.8 rgbPath_rygcbm
            , rgbCubePathScaled 4 0.8 rgbPath_rygcbm
            , rgbCubePathScaled 1 0.8 rgbPath_diags1
            , rgbCubePathScaled 2 0.8 rgbPath_diags1
            , rgbCubePathScaled 4 0.8 rgbPath_diags1
            ]
        modes = map (\s -> putBox (sgrRGB s)) colorSchemes
    in mapM_ (\m -> display m 1 0 background >> putStrLn "") modes


-- Exploratory functions
-----------------------------------------------------------

sortByCount :: Ord a => [(a, Int, [Bluebird.NatWord])] -> [(a, Int, [Bluebird.NatWord])]
sortByCount = sortOn (\(c, _, _) -> c)

sortByLength :: [(Int, Int, [Bluebird.NatWord])] -> [(Int, Int, [Bluebird.NatWord])]
sortByLength = sortOn (\(_, l, _) -> l)

displayUniqueCycles :: ([(Int, Int, [Bluebird.NatWord])] -> [(Int, Int, [Bluebird.NatWord])])
    -> (Natural -> IO ()) -> Int -> Bluebird.NatWord -> [Bluebird.NatWord] -> IO ()
displayUniqueCycles transform mode n c w0s = do
    let stats = transform $ uniqueCycles n c w0s
        phis = map (\(_, _, c) -> map phi c) stats
        eta_c = if length c > 0 then eta (head c) else eta 0
        etas = map (\(_, _, w) -> map eta_c w) stats
        action = \((count, length, cycle), phis, etas) -> do
            putStrLn "------------------------------------------"
            putStrLn $ "(Count, Length) = " ++ show (count, length)
            putStrLn $ "Phis = " ++ show phis
            putStrLn $ "Etas = " ++ show etas
            putStrLn ""
            display mode 1 0 cycle
            putStrLn ""
    mapM_ action $ zip3 stats phis etas


displayFirstNUpToCycle :: (Natural -> IO ()) -> Int -> Bluebird.NatWord -> [Bluebird.NatWord] -> IO ()
displayFirstNUpToCycle mode n c w0s = do
    let stats = zip (cycles c w0s) (generations c w0s)
        action = \((start, length), gens) -> do
            putStrLn "------------------------------------------"
            putStrLn $ "(Start, Length) = " ++ show (start, length)
            putStrLn $ "w0 = " ++ show (head gens)
            putStrLn ""
            display mode 1 0 $ take (start + length) gens
            putStrLn ""
    mapM_ action $ take n stats
    

divInts :: Int -> Int -> Double
divInts a b = (fromIntegral a) / (fromIntegral b)
    
printUniqueCyclesPercentages :: Int -> Bluebird.NatWord -> [Bluebird.NatWord] -> IO ()
printUniqueCyclesPercentages n c w0s =
    let absolutes = uniqueCycles n c w0s
        percentages = (\(a, b, c) -> (divInts a n, b, c))
        printer = \(freq, len, _) -> printf "(%.3f, %d)\n" freq len
        relatives = map percentages absolutes
    in mapM_ printer $ reverse $ sortByCount relatives
        
main = return ()
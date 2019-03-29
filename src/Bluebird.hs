module Bluebird where

import Control.Monad
import Data.List
import Data.List.Split (chunksOf)
import Data.List.Unique
import Data.Natural
import Data.Maybe


-- List utility functions
---------------------------------------------------

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

atIndices :: [Int] -> [a] -> [a]
atIndices is xs = map (xs !!) is

findFirstEqIndex :: Eq a => [a] -> [a] -> Maybe Int
findFirstEqIndex xs ys = 
    findIndex (\p -> fst p == snd p) $ zip xs ys

-- Floyd's Tortoise and Hare Cycle Finding
--  https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
--  Assumes the list is of the form
--      [x, f x, f f x, ...]
findCycle :: Eq a => [a] -> (Int, Int)
findCycle xs =
    let -- (1) Find smallest m : x(m) == x(2m)
        -- Loops infinitely if m does not exist
        -- 1 is added to m since tort starts at x(1)
        tort = tail xs
        hare = atIndices (map (* 2) [1..]) xs
        m = 1 + (fromMaybe 0 $ findFirstEqIndex tort hare)
        
        -- (2) Find the smallest k: x(k) == x(m+k) (first rep)
        -- This is guaranteed to exist at this point
        xs' = drop m xs
        k = fromMaybe 0 $ findFirstEqIndex xs xs'
        
        -- (3) Find smallest 0 < c <= k: x(m) == x(m+c)
        -- If not found, c <- m
        c = if k == 0
            then m
            else
                let xs'' = tail $ take k xs'
                    x_m = xs !! m
                in 1 + (fromMaybe (m - 1) $ findIndex ((==) x_m) xs'')
    in (k, c)
        

setAt :: Int -> a -> [a] -> [a]
setAt i n w =
    let u = take i w
        v = drop (i + 1) w
    in u ++ [n] ++ v
    

-- The Bluebird Combinator String Rewrite System
---------------------------------------------------
type NatWord = [Natural]

rewrite :: NatWord -> NatWord
rewrite w =
    let mismatches = elemIndices True $ zipWith (<) w $ tail w
    in  if mismatches == []
        then map (\n -> monus n 1) $ takeWhile (\n -> n > 0) w
        else
            let op = \w m ->
                    let a = w !! m
                        b = w !! (m + 1)
                    in setAt m (b + 1) $ setAt (m + 1) a w
            in rewrite $ foldl op w mismatches
            
        
reductions :: NatWord -> NatWord -> [NatWord]
reductions c w = 
    let c' = map (+ 1) c
    in iterate (\v -> rewrite (v ++ c')) w


-- Variations of initial generations
---------------------------------------------------

-- [], [c], [c, c], [c, c, c], ...
w0_constInits :: Natural -> [NatWord]
w0_constInits c = inits $ repeat c

-- [], [0], [0, 1], [0, 1, 2], ...
w0_ascending :: [NatWord]
w0_ascending = inits [0..]

-- [], [0], [1, 0], [2, 1, 0], ...
w0_descending :: [NatWord]
w0_descending = map reverse w0_ascending

-- [0], [1], [2], ...
w0_nats :: [NatWord]
w0_nats = iterate (\w -> [succ (head w)]) [0]

-- [ [] ],
-- [ [0], [1], [2], ... ]
-- [ [0, 0], [1, 0], [1, 1], ... ]
-- ...
w0_qlexDecr_n :: Natural -> [NatWord]
w0_qlexDecr_n 0 = [[]]
w0_qlexDecr_n 1 = w0_nats
w0_qlexDecr_n n =
    let gen = \m ->
            let w0_prev = w0_qlexDecr_n $ n - 1
                suffixes = takeWhile (\w -> head w <= m) w0_prev
            in map (\w -> m : w) suffixes
    in concat $ map gen [0..]
    
dovetail :: [(Int, Int)]
dovetail = iterate next (0, 0) where
    next = \(x, y) ->
        if y == 0
        then
            if x `mod` 2 == 0
            then (x + 1, y)
            else (x - 1, y + 1)
        else 
            if x == 0
            then
                if y `mod` 2 /= 0
                then (x, y + 1)
                else (x + 1, y - 1)
            else
                if (x + y) `mod` 2 == 0
                then (x + 1, y - 1)
                else (x - 1, y + 1)

-- Dovetailing on w0_qlexDecr_n
w0_qlexDecr_dove :: [NatWord]                
w0_qlexDecr_dove =
    let w0s_fixedLens = map w0_qlexDecr_n [1..]
        op = \(len, idx) -> (w0s_fixedLens !! len) !! idx
    in [] : (map op dovetail)
    
w0_dove = w0_qlexDecr_dove

-- All w0 with phi(w0) <= n
w0_phi_leq :: Natural -> [NatWord]
w0_phi_leq n =
    let w0s_fixedLens = map w0_qlexDecr_n [0..n]
        w0s_belowPhi = map (takeWhile (\w -> phi w <= n)) w0s_fixedLens
    in concat w0s_belowPhi

-- Exploratory functions
---------------------------------------------------
generations c iws = map (reductions c) iws

cycles_n :: Natural -> [(Int, Int)]
cycles_n n = map findCycle $ map (reductions [n]) $ inits $ repeat n

cycles :: NatWord -> [NatWord] -> [(Int, Int)]
cycles c iws = map findCycle $ map (reductions c) iws

cycleSequences :: NatWord -> [NatWord] -> [[NatWord]]
cycleSequences c iws =
    let crep = cycles c iws
        gens = map (reductions c) iws
        lens = map (\p -> fst p + snd p) crep
    in zipWith (drop . fst) crep $ zipWith take lens gens

absPointwiseDist :: NatWord -> NatWord -> NatWord
absPointwiseDist w v =
    let dist = \n m -> max (monus n m) (monus m n)
    in zipWith dist w v
    
chunkDist :: [NatWord] -> [NatWord] -> [NatWord]
chunkDist c d = zipWith absPointwiseDist c d

adjacentChunkDistances :: Int -> [NatWord] -> [[NatWord]]
adjacentChunkDistances n ws =
    let chunks = chunksOf n ws
    in zipWith chunkDist chunks $ (tail chunks) ++ [head chunks]

isShiftedVersion :: Eq a => [a] -> [a] -> Bool
isShiftedVersion ws vs = 
    if length ws /= length vs
    then False
    else
        let shifts = [0..((length ws) - 1)]
            shiftedVs = map (\n -> rotate n vs) shifts
            comparisons = zipWith (==) (repeat ws) shiftedVs
        in True `elem` comparisons
        
-- Returned is a list of tuples of the form
--      (count, length, cycle)
cyclesStats :: Int -> NatWord -> [NatWord] -> [(Int, Int, [NatWord])]
cyclesStats n c w0s =
    let counts = count_ $ take n $ cycleSequences c w0s
        op = \p -> (snd p, length (fst p), fst p)
    in map op counts

-- Like cyclesStats but accumulates counts for equivalent cycles
uniqueCycles :: Int -> NatWord -> [NatWord] -> [(Int, Int, [NatWord])]
uniqueCycles n c w0s =
    let cStats = cyclesStats n c w0s
        eq = \(_, _, a) (_, _, b) -> isShiftedVersion a b
        fop = \uStats (c_a, l_a, a) ->
            case findIndex (eq (c_a, l_a, a)) uStats of
                Just i ->
                    let (c_b, _, b) = uStats !! i
                    in setAt i (c_a + c_b, l_a, b) uStats
                Nothing ->
                    uStats ++ [(c_a, l_a, a)]
    in foldl fop [] cStats
    
-- Measure function used for termination proof
phi :: NatWord -> Natural
phi w = (sum w) + (genericLength w)

eta :: Natural -> NatWord -> Int
eta c w = fromMaybe (length w) $ findIndex 
    (\(w_i, i) -> w_i > i + c) $ zip (reverse w) [0..((genericLength w) - 1)]

upToFirstRepetition :: [NatWord] -> [NatWord]
upToFirstRepetition w = 
    let (start, length) = findCycle w
    in take (start + length) w


-- Display helpers
---------------------------------------------------
peek :: Show a => Int -> Int -> [a] -> IO ()
peek start n xs = mapM_ print $ drop start $ take (start + n) xs
    
showPhiEtaReductions :: Int -> Natural -> NatWord -> IO ()
showPhiEtaReductions n c w = peek 0 n $
    let rs = reductions [c] w
    in zip3 (map phi rs) (map (eta c) rs) rs

-- Utility function to hide the third component of cyclestats
hideCycleSeq :: (a, b, c) -> (a, b)
hideCycleSeq (a, b, _) = (a, b)
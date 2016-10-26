{-# LANGUAGE RankNTypes #-}
module Fadno.Util where

import Control.Applicative
import Control.Monad (replicateM)
import Test.HUnit
import Data.List
import Data.Function (on)
import qualified Debug.Trace as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Lens

-- compute intervals
diff :: Num a => [a] -> [a]
diff = zipTail (flip (-))

-- a simple reducer.
-- quicksilver says: zip`ap`tail - the Aztec god of consecutive numbers
zipTail :: (a -> a -> c) -> [a] -> [c]
zipTail f = zipWith f <*> tail

-- opposite of diff, compute concrete notes from intervals
integ :: Int -> [Int] -> [Int]
integ = scanl (+)

-- | 'Debug.Trace.trace' with brackets.
trace :: String -> a -> a
trace s = T.trace ("<" ++ s ++ ">")
-- | trace with output of result.
trace' :: Show a => String -> a -> a
trace' s a = trace1 s a a
-- | trace with extra variable, not showing result.
trace1 :: Show b => String -> b -> a -> a
trace1 s a = trace (s ++ ":" ++ show a)
-- | trace with extra variable and output of result.
trace1' :: (Show a, Show b) => String -> b -> a -> a
trace1' s a b = trace (s ++ ":" ++ show a ++ "," ++ show b) b



-- | pop items off a stateful list, use for monadic function.
popping :: (MonadState s m) => Int -> Lens' s [a] -> ([a] -> m b) -> m b
popping n l f = do
  as <- use l
  l .= drop n as
  f (take n as)

-- | 'popping' but only runs function if popped items are non-empty
popping' :: (MonadState s m) => Int -> Lens' s [a] -> ([a] -> m b) -> m (Maybe b)
popping' n l f = popping n l
                     (\as -> if null as then return Nothing else Just <$> f as)

-- | popping with only head
popping1 :: (MonadState s m) => Lens' s [a] -> (a -> m b) -> m (Maybe b)
popping1 l f = popping' 1 l (f . head)

-- | 'succ' with wraparound.
wrapSucc :: (Bounded a, Enum a, Eq a) => a -> a
wrapSucc s = if s == maxBound then minBound else succ s
-- | 'pred' with wraparound.
wrapPred :: (Bounded a, Enum a, Eq a) => a -> a
wrapPred s = if s == minBound then maxBound else pred s

-- | do monadic 'over' -- '(%=)' -- with pass-through of (before,after)
mutating :: MonadState s m => Lens' s a -> (a -> a) -> m (a,a)
mutating l f = do
  a <- use l
  a' <- l <.= f a
  return (a,a')

-- | reorganize 'maybe' for chaining on Just
maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m n j = maybe n j m

median :: Integral a => [a] -> Maybe a
median [] = Nothing
median ls = Just $ if odd len then sorted !! mid
                   else (sorted !! mid + sorted !! (mid - 1)) `div` 2
    where len = length ls
          sorted = sort ls
          mid = len `div` 2





-- subtract all by minimum to "normalize" around 0
normalize :: (Num a, Ord a) => [a] -> [a]
normalize l = map (flip (-) $ minimum l) l

-- PC rules state you must rotate the scale through the gamut,
-- and selecting for the least distance from tail -> head, tail-1 -> head, etc.
-- Line can be any melody, gets normalized to gamut.
pitchClassSet :: Int -> [Int] -> [Int]
pitchClassSet gamut line = let
    modg = flip mod gamut
    norm = normalize . sort . nub . map modg $ line
    -- rotate through gamut
    alts = nub $ map (\x -> normalize . sort $ map (modg . (x+)) norm) [0..(gamut-1)]
    -- compute "values" as distance from head, reversed
    vals = map (\x -> reverse $ map (flip (-) (head x)) x) alts
    min = minimum vals
    in fst . minimumBy (compare `on` snd) $ zip alts vals


lfsr :: Int -> Int -> Int -> [Bool]
lfsr len tap1 tap2 =
    if len<tap1 || len<tap2 || tap1==tap2 || len<2 then
        error ("lfsr: invalid arguments: " ++ show [len,tap1,tap2])
    else
        map snd . shift $ replicate len True where
            shift r = v:next v where v = (r, r !! tap1 /= r !! tap2)
            next (register,prev) = shift $ prev:register

-- | generate "A" .. "Z", "AA" .. "AZ", "BA" .. "BZ", .. "AAA" etc
rehMarks :: [String]
rehMarks = a ++ ((++) <$> a <*> a) where a = map (:[]) ['A'..'Z']

-- apply 'i' rotations to list
rotate :: Int -> [a] -> [a]
rotate i l = zipWith const (drop i $ cycle l) l

-- get all rotations of a list
rotations :: [a] -> [[a]]
rotations l = flip rotate l <$> [1..(length l)]

-- Cartesian product of specified dimension
allTuples :: Int -> [a] -> [[a]]
allTuples = replicateM

monotonic :: [Int] -> Bool
monotonic = (2 >) . length . nub . filter (EQ /=) . zipTail compare

interleave :: [[a]] -> [a]
interleave = concat . pivot

pivot :: [[a]] -> [[a]]
pivot chords = map iter [0..maxLength] where
    maxLength = minimum (map length chords) - 1
    iter i = map (!! i) chords

filterOnKeys :: (Ord a) => [a] -> M.Map a b -> M.Map a b
filterOnKeys ks = M.filterWithKey (\k _ -> S.member k $ S.fromList ks)

pairBy :: (a -> b) -> [a] -> [(a,b)]
pairBy f = map (\a -> (a,f a))

delim :: String -> [String] -> String
delim _ []              =  ""
delim _ [w]             = w
delim d (w:ws)          = w ++ d ++ delim d ws

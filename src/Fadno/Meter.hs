{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Fadno.Meter where

import Fadno.Notation
import Fadno.Note
import Control.Lens hiding (Empty,pre)
import Data.Foldable
import Test.HUnit
import Data.Ratio
import GHC.Real
import Data.Maybe
import Data.List (sort,sortBy,nub)
import Data.Function
import Control.Arrow
import qualified Data.Map.Strict as M
import Safe
import Fadno.Util

type HasRatioNotes t n p = (Traversable t, HasNote n p Rational)

--
-- REBAR
--

-- | use 'rebar' with multiple input "bars".
rebars :: (HasRatioNotes b n p, HasRatioNotes c m p,Monoid p,Eq p,
           Monoid (c m), Monoid (b n),HasTimeSignature (c m),
           Snoc (c m) (c m) m m,HasTie m,Show (c m)) =>
          TimeSignature -> [b n] -> [c m]
rebars ts = rebar ts . mconcat

-- | Given a time signature and a "bar" (Traversable "b" of HasNotes "n"),
-- make new "bars" (Traversable "c" of HasNotes "m"),
-- partitioning notes, applying ties as needed,
-- and decorating with the time signature.
rebar :: (HasRatioNotes b n p, HasRatioNotes c m p,Monoid p,Eq p,Monoid (c m),
          HasTimeSignature (c m),Snoc (c m) (c m) m m,HasTie m,Show (c m)) =>
         TimeSignature -> b n -> [c m]
rebar ts = reverse . fixup . foldl' go [mempty] . fmap fromNote where
    tslen = tsToRatio ts
    go [] _ = error "impossible"
    go bss@(b:bs) n | barlen == tslen = go (mempty:bss) n
                    | newBarLen <= tslen = (b |> n):bs
                    | otherwise = {- trace1' "otherwise" (remaining, ndur) $ -} go ((b |> pre):bs) post
        where barlen = sumDurs b
              ndur = view noteDur n
              newBarLen = barlen + ndur
              remaining = newBarLen - tslen
              post = tieMay TStop . set noteDur remaining $ n
              pre = tieMay TStart . set noteDur (ndur - remaining) $ n
    -- fixup: set head and tail timesigs
    fixup [] = []
    fixup bs = over _head fixLast . joinLast . over _last (timeSignature ?~ ts) $ bs
    fixLast b | barlen == tslen = b
              | otherwise = case tsFromRatio' ts barlen of
                              Just t -> b & timeSignature ?~ t
                              Nothing -> b
              where barlen = sumDurs b
    -- joinLast: apply heuristic that an additional bar less than 1/2 the ts length
    -- should be merged with the prior bar.
    joinLast aas@(a:b:as) | sumDurs a + sumDurs b < tslen * (3%2) =
                              (b `mappend` a):as -- leave notes tied. why not.
                          | otherwise = aas
    joinLast a = a



-- | 'rebar' using 'Bar' and 'Note\'' for output.
rebar' :: (HasRatioNotes b n p,Monoid p,Eq p,Show p) =>
          TimeSignature -> b n -> [Bar (Note' p Rational)]
rebar' = rebar

-- | 'rebars' using 'Bar' and 'Note\'' for output.
rebars' :: (HasRatioNotes b n p,Monoid (b n),Monoid p,Eq p,Show p) =>
           TimeSignature -> [b n] -> [Bar (Note' p Rational)]
rebars' = rebars

-- | Set tie if not a rest
tieMay :: (Eq a, Monoid a, HasNote s a d, HasTie s) => Tie -> s -> s
tieMay end v | isRest v = v
             | otherwise = over tie setTie v
             where setTie o@(Just old) | old == end = o
                                       | otherwise = Just TBoth
                   setTie Nothing = Just end



--
-- TIE RULES
--

-- | Representable duration denominators.
--
-- For standard base-2 durs, 2 and 4 are spurious as they reduce to 1,
-- thus 1 plus the "dot values" 3,7.
--
-- For non-standard (quintuples etc), we admit 2 and 4 as well, for e.g. 2%5,
-- a "half-note" under a quarter-quintuple. Anything greater
-- exceeding the understanding limit: 8%17 can certainly be represented as
-- a half-note, but it makes little sense to the reader.
validDenoms :: [Integer]
validDenoms = [1,2,3,4,7]

-- | Max representable duration.
maxDur :: Rational
maxDur = 2

-- | Test for representational duration per 'validDenoms' and 'maxDur'.
validDur :: Rational -> Bool
validDur r = r == maxDur ||
             (r < maxDur && numerator r `elem` validDenoms)

-- | Tie rules that work across any denominators, such that
-- 5%8 -> [1%2,1%8], 9%16 -> [1%2,1%16],  11%16 -> [1%2,3%16],
-- 13%16 -> [3%2,1%16], 9%4 -> [2,1%4].
splitDur :: Rational -> [Rational]
splitDur r | r < 0 = error "splitDur: negative duration"
           | r == 0 = []
           | validDur r = [r]
           -- NB: subtraction doesn't preserve denom (17:%20 - 3%5 -> 1%4, not 5%20)
           | otherwise = split:splitDur (r - split)
           where split = findSplit r

-- | Find split by 1) finding largest power-of-2 fraction under value or
-- 2) finding longest power-of-two denominator split, up to 8.
findSplit :: Rational -> Rational
findSplit r = case filter validDur candidates of
                [] -> splitOnValid
                (v:_) -> v
    where
      n = numerator r
      d = denominator r
      pow2s = [x | p <- [(1 :: Integer)..], x<-[2^p]]
      denomPow2s = reverse $ takeWhile (\v -> v < d && d `rem` v == 0) pow2s
      candidates = filter (<r) $ map (\cd -> (n * cd `div` d) :% (d `div` cd)) denomPow2s
      splitOnValid = case filter (<= min r maxDur) $ map (:%d) $ reverse $
                     takeWhile (<min n 8) pow2s of
                       [] -> r
                       (v:_) -> v

-- | Apply rules in 'splitDur' and tie affected notes.
tieRules :: (HasRatioNotes b n p, HasTie n, Monoid p, Eq p, Show n,
             HasRatioNotes c m p, HasTie m, Monoid (c m),
             Snoc (c m) (c m) m m) => b n -> c m
tieRules = foldl' apply mempty where
    apply r n = case splitDur (view noteDur n) of
                       [] -> error $ "tieRules: empty result from splitDur for " ++ show n
                       [_] -> r |> set tie (view tie n) (fromNote n)
                       ds -> foldl (|>) r . fixLast . fixFirst . map mkTied $ ds
                           where mkTied d = tieMay TBoth (set noteDur d (fromNote n))
                                 forOrgTie t = case view tie n of
                                                 Nothing -> t
                                                 (Just a) | a == t -> t
                                                          | otherwise -> TBoth
                                 fixFirst = over (_head.tie) (fmap (const (forOrgTie TStart)))
                                 fixLast = over (_last.tie) (fmap (const (forOrgTie TStop)))



-- | Monomorphic-result 'tieRules
tieRules' :: (HasRatioNotes b n p, HasTie n, Monoid p, Eq p, Show n) =>
             b n -> Bar (Note' p Rational)
tieRules' = tieRules


--
-- SELECT TIMESIG
--

-- | Weights and pulse values for pre-configured TSs.
data TsConfig = TsConfig {
      _tSig :: TimeSignature
    , _tWeight :: Rational
    , _tPulse :: Rational
    } deriving (Eq,Show)
makeLenses ''TsConfig

selectTimeSig :: HasRatioNotes t n p => [t n] -> Maybe TimeSignature
selectTimeSig phrases = fmap fst $ headMay $ selectTimeSigs phrases

-- | Combine scores from phrases.
selectTimeSigs :: HasRatioNotes t n p => [t n] -> [(TimeSignature,Rational)]
selectTimeSigs = mergeScores . preferDivisableHeads . map selectTsConfigs where
    mergeScores = sortBy (flip compare `on` snd) .
                  M.toList . foldl1 (M.unionWith (+)) .
                  map (M.fromListWith max . map (_tSig &&& _tWeight))


-- | nutty heuristic that overweights a TS for a uniform duration divisor
preferDivisableHeads :: [[TsConfig]] -> [[TsConfig]]
preferDivisableHeads [] = []
preferDivisableHeads [a] = [a]
preferDivisableHeads phraseTss =
    case sequence (map headMay phraseTss) of
      Nothing -> phraseTss
      Just heads | length (nub heads) == 1 -> phraseTss
                 | otherwise -> case nub $ (zipWith commonDivHeur <*> tail) (map tsConfigToDur heads) of
                      [] -> phraseTss
                      [a] -> maybe phraseTss ((:phraseTss).return) $ tsConfigFromDur 100 a
                      _ -> phraseTss

-- | main heuristic is finding the common divisible duration,
-- with requirement that it must be greater than 1/4 the difference between the durations.
-- Hopefully avoids crappy tiny TSs like 2/8.
commonDivHeur :: Rational -> Rational -> Rational
commonDivHeur d1 d2 | d1 == d2 = d1
                | c / (abs (d1 - d2)) > (1%4) = min d1 d2
                | otherwise = c
                where c = d1 / fromIntegral (numerator (d1/d2))


tsConfigToDur :: TsConfig -> Rational
tsConfigToDur = tsToRatio . _tSig



-- | Attempt to construct a TS config from duration
tsConfigFromDur :: Rational -> Rational -> Maybe TsConfig
tsConfigFromDur weight = fmap (\t -> TsConfig t weight (minMedianDur (_tsUnit t))) . tsFromRatio

-- | Pre-configured timesigs.
tsConfigs :: [TsConfig]
tsConfigs = [TsConfig (4/:Q4) 9 (1%4)
            ,TsConfig (3/:Q4) 8 (1%4)
            ,TsConfig (6/:Q8) 8 (3%8)
            ,TsConfig (12/:Q8) 7 (3%8)
            ,TsConfig (2/:Q4) 6 (1%4)
             ,TsConfig (5/:Q4) 5 (5%4)
             ,TsConfig (5/:Q8) 5 (5%8)
             ,TsConfig (7/:Q4) 5 (7%4)
            ,TsConfig (7/:Q8) 5 (7%8)
             ,TsConfig (9/:Q8) 5 (3%8)
            ]


-- | Given a median note duration, minima for acceptable quanta.
minMedianDur :: Quanta -> Rational
minMedianDur q = fromMaybe (1%32) . lookup q $
                                   [(Q8,1%32),(Q4,1%16),(Q2,1%4)]

-- | Given a phrase, select configs
selectTsConfigs :: HasRatioNotes t n p => t n -> [TsConfig]
selectTsConfigs phrase | null phrase = []
                       | otherwise = sortBy (flip compare `on` _tWeight) $
                                     mapMaybe (evalTsConfig phrase)
                                     -- append custom-length TS as lowest-weight choice
                                     (maybe tsConfigs (:tsConfigs) (tsConfigFromDur 4 (sumDurs phrase)))

-- | Filter and score time signatures per heuristics.
evalTsConfig :: HasRatioNotes t n p => t n -> TsConfig -> Maybe TsConfig
evalTsConfig phrase c@(TsConfig ts@(TimeSignature _n q) _ pulse)
    | medianDur < minMedianDur q = Nothing -- density filter
    | phraseDur < tsDur = Nothing -- min duration filter
    | tsDur >= 2 = Nothing -- too long TS
    | phraseDur == tsDur = Just $ over tWeight (* (9%4)) c -- exact length match bonus,
    | otherwise = Just $ over tWeight computeWeight c
        where medianDur = sort (toListOf (traverse.noteDur) phrase) !! (phraseLength `div` 2)
              phraseLength = length phrase
              phraseDur = sumDurs phrase
              tsDur = tsToRatio ts
              -- scale by pulse coverage, subtract by divisibility by ts duration
              computeWeight w = (w * pulseCoverage pulse phrase) - (phraseDur `frem` tsDur)

-- | 'rem' for 'RealFrac'
frem :: RealFrac a => a -> a -> a
frem a b = let (_ :: Int,f) = properFraction (a/b) in f

isDivBy :: RealFrac a => a -> a -> Bool
isDivBy a b = 0 == frem a b

-- | Compute percentage of notes falling on pulse values.
pulseCoverage :: HasRatioNotes t n p => Rational -> t n -> Rational
pulseCoverage pulse phrase = fromIntegral pulseNoteCount % fromIntegral (length phrase)
    where
      pulseNoteCount = length . filter ((`isDivBy` pulse) . fst) . mapTime $ phrase

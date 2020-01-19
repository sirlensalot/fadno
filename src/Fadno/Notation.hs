{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fadno.Notation where

import GHC.Generics
import Data.String
import Data.Default
import Fadno.Note
import Control.Lens hiding (pre)
import Data.Typeable
import Data.Ratio
import Data.Sequence (Seq,fromList)
import Data.Foldable
import Test.HUnit
import Data.List
import Data.Maybe


-- valid time sig denoms
data Quanta = Q2|Q4|Q8|Q16|Q32|Q64
               deriving (Eq,Show,Ord,Enum,Bounded,Typeable)

qToInt :: Quanta -> Int
qToInt = (2^) . succ . fromEnum
qFromInt :: Integral i => i -> Maybe Quanta
qFromInt = fmap toEnum . (`elemIndex` [2,4,8,16,32,64])


data TimeSignature = TimeSignature { _tsLength :: Int, _tsUnit :: Quanta }
   deriving (Eq,Ord)
instance Show TimeSignature where
    show (TimeSignature l u) = show l ++ "/:" ++ show u
makeLenses ''TimeSignature
class HasTimeSignature a where timeSignature :: Lens' a (Maybe TimeSignature)

(/:) :: Int -> Quanta -> TimeSignature
(/:) = TimeSignature

-- PPQ: valid midi divisions, named after equivalent Quantum
-- as in, "1 means ..."; PQ4 is "1 means quarter note"
data PPQ = PQ4|PQ8|PQ16|PQ32|PQ64|PQ128|PQ256
         deriving (Eq,Show,Ord,Enum,Bounded)


-- convert to midi division value
ppqDiv :: Integral a => PPQ -> a
ppqDiv = (2^) . fromEnum


-- Compute duration of TS
tsToRatio :: TimeSignature -> Rational
tsToRatio (TimeSignature n d) = fromIntegral n % fromIntegral (qToInt d)

-- Derive TS from duration, with 1 denominator implying Q4
tsFromRatio :: Rational -> Maybe TimeSignature
tsFromRatio r = toTs (if d == 1 then 4 else (if n == 1 then 2 else 1))
    where toTs m = (fromIntegral (n * m) /:) <$> qFromInt (d * m)
          d = denominator r
          n = numerator r


tsFromRatio' :: TimeSignature -> Rational -> Maybe TimeSignature
tsFromRatio' (TimeSignature _ src) = fmap adjust . tsFromRatio where
    adjust t@(TimeSignature n d) | src <= d = t
                                 | otherwise = (n * m) /: src
                                 where qd = qToInt d
                                       qs = qToInt src
                                       m = qs `div` qd


-- | Duration iso, from Integral to Rational, given PPQ
ratioPPQ :: Integral a => PPQ -> Iso' a Rational
ratioPPQ p = iso toRat toInt where
    ppq4 = ppqDiv p * 4
    toRat i = fromIntegral i % fromIntegral ppq4
    toInt r = truncate (r * toRational ppq4)

-- | Adapt a type to its HasXXX "Maybe Lens'"
adaptHas :: Lens' a (Maybe a)
adaptHas f s = fromMaybe s <$> f (Just s)

-- | Adapt a non-Maybe lens to the HasXXX "Maybe Lens'"
adaptHasLens :: Lens' s a -> Lens' s (Maybe a)
adaptHasLens l f s = fmap (maybe s (\a -> set l a s)) (f (Just (view l s)))

-- | Adapt a type that does NOT support the HasXXX feature.
adaptHasNot :: Lens' s (Maybe a)
adaptHasNot f s = fmap (const s) (f Nothing)


-- | Tied notes.
data Tie = TStart | TStop | TBoth
    deriving (Eq,Bounded,Enum,Ord,Show)
makeLenses ''Tie
class HasTie a where tie :: Lens' a (Maybe Tie)
instance HasTie Tie where tie = adaptHas
instance HasTie (Note p d) where tie = adaptHasNot


-- | Slurred notes.
data Slur = SStart | SStop
    deriving (Eq,Bounded,Enum,Ord,Show)
makeLenses ''Slur
class HasSlur a where slur :: Lens' a (Maybe Slur)
instance HasSlur Slur where slur = adaptHas

-- | Note articulations.
data Articulation = Staccato | Accent
    deriving (Eq,Show,Bounded,Enum,Ord)
class HasArticulation a where articulation :: Lens' a (Maybe Articulation)
instance HasArticulation Articulation where articulation = adaptHas


-- | Bar rehearsal mark.
newtype RehearsalMark = RehearsalMark { _rehearsalText :: String }
    deriving (Eq,Ord,IsString,Generic,Semigroup,Monoid,Default)
makeLenses ''RehearsalMark
instance Show RehearsalMark where show = show . _rehearsalText
class HasRehearsalMark a where rehearsalMark :: Lens' a (Maybe RehearsalMark)
instance HasRehearsalMark RehearsalMark where rehearsalMark = adaptHas

-- | Musical direction.
newtype Direction = Direction { _directionText :: String }
    deriving (Eq,Ord,IsString,Generic,Semigroup,Monoid,Default)
makeLenses ''Direction
instance Show Direction where show = show . _directionText
class HasDirection a where direction :: Lens' a (Maybe Direction)
instance HasDirection Direction where direction = adaptHas

-- | Barline.
data Barline = Double | Final
    deriving (Eq,Show,Ord,Generic)
class HasBarline a where barline :: Lens' a (Maybe Barline)
instance HasBarline Barline where barline = adaptHas

data Repeats = RStart | REnd | RBoth
    deriving (Eq,Show,Ord,Generic)
class HasRepeats a where repeats :: Lens' a (Maybe Repeats)
instance HasRepeats Repeats where repeats = adaptHas

data Clef = TrebleClef | BassClef | AltoClef | PercClef
    deriving (Eq,Show,Ord,Generic)
makeLenses ''Clef
class HasClef a where clef :: Lens' a (Maybe Clef)
instance HasClef Clef where clef = adaptHas


-- | Part identifier, prefers 'Num' or 'IsString' values.
newtype Part a = Part { _partIdx :: a }
    deriving (Eq,Generic,Ord,Functor,Bounded,Foldable,Traversable,Real,Num,IsString)
makeLenses ''Part
instance (Show a) => Show (Part a) where show = show._partIdx
class HasPart a b | a -> b where part :: Lens' a (Maybe (Part b))

-- | Lensy show of a Maybe field, given a 'Getter' and its name.
mshow :: (Show a) => Getter s (Maybe a) -> String -> s -> String
mshow l n = maybe "" (\v -> " & " ++ n ++ " ?~ " ++ show v) . view l

-- | 'concatMap' show functions with a prelude.
mshows :: s -> String -> [s -> String] -> String
mshows s pre = (pre ++) . concatMap ($ s)

-- Example types.

-- | Note with notations.
data Note' p d = Note' {
      _nNote :: Note p d
    , _nTie :: Maybe Tie
    , _nSlur :: Maybe Slur
    , _nArticulation :: Maybe Articulation
    } deriving (Eq,Generic)

makeLenses ''Note'
instance HasNote (Note' p d) p d where
    note = nNote
    fromNote = note' . view note
instance HasTie (Note' p d) where tie = nTie
instance HasSlur (Note' p d) where slur = nSlur
instance HasArticulation (Note' p d) where articulation = nArticulation
instance (Show p, Show d) => Show (Note' p d) where
    show n = mshows n ("note' (" ++ show (view nNote n) ++ ")")
             [mshow tie "tie"
             ,mshow slur "slur"
             ,mshow articulation "articulation"
             ]


-- | Note smart ctor, used in 'Show'.
note' :: Note p d -> Note' p d
note' n = Note' n Nothing Nothing Nothing

testNote :: Note' [Int] Int
testNote = note' ([60]|:2) & tie ?~ TStart & articulation ?~ Accent

-- | Bar as list of notes, with notations.
data Bar n = Bar {
      _bNotes :: Seq n
    , _bRehearsalMark :: Maybe RehearsalMark
    , _bDirection :: Maybe Direction
    , _bBarline :: Maybe Barline
    , _bRepeats :: Maybe Repeats
    , _bTimeSignature :: Maybe TimeSignature
    , _bClef :: Maybe Clef
    } deriving (Eq,Generic,Functor,Foldable,Traversable)
makeLenses ''Bar
instance Default (Bar n) where def = bar []
instance Snoc (Bar n) (Bar n) n n where
    _Snoc = prism (\(b,n) -> over bNotes (review _Snoc . (,n)) b) $
            \b -> case firstOf _Snoc (view bNotes b) of
                    Nothing -> Left (def :: Bar n)
                    (Just (as,a)) -> Right (set bNotes as b,a)
instance Cons (Bar n) (Bar n) n n where
    _Cons = prism (\(n,b) -> over bNotes (review _Cons . (n,)) b) $
            \b -> case firstOf _Cons (view bNotes b) of
                    Nothing -> Left (def :: Bar n)
                    (Just (a,as)) -> Right (a,set bNotes as b)
instance HasRehearsalMark (Bar n) where rehearsalMark = bRehearsalMark
instance HasDirection (Bar n) where direction = bDirection
instance HasBarline (Bar n) where barline = bBarline
instance HasTimeSignature (Bar n) where timeSignature = bTimeSignature
instance HasClef (Bar n) where clef = bClef
instance HasRepeats (Bar n) where repeats = bRepeats
instance (Show n) => Show (Bar n) where
    show b = mshows b ("bar " ++ show (toList $ view bNotes b))
             [mshow rehearsalMark "rehearsalMark"
             ,mshow direction "direction"
             ,mshow barline "barline"
             ,mshow repeats "repeat"
             ,mshow timeSignature "timeSignature"
             ,mshow clef "clef"
             ]
instance Semigroup (Bar n) where
    a <> b = over bNotes (<> view bNotes b) a
instance Monoid (Bar n) where
    mempty = def

-- | Bar smart ctor, used in 'Show'.
bar :: [n] -> Bar n
bar ns = Bar (fromList ns) Nothing Nothing Nothing Nothing Nothing Nothing

testBar :: Bar (Note [Int] Int)
testBar = bar [[60]|:2,[62]|:1] & timeSignature ?~ TimeSignature 4 Q4 & direction ?~ "Softly"

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Fadno.Note
    (Note(..),pitch,dur
    ,HasNote(..),toPair,(|:)
    ,Mono(..),maybeMono,mono,mono',mPitch,unMono,catMonos,_M
    ,rest,isRest
    ,Spelling(..),fromChroma,toChroma,spelling
    ,PitchRep(..),prPitch,prOctave,(@:),pitchRep
    ,sumDurs,mapTime
    ,tied,tied',legato,legato',merge
    ,transpose,transpose'
    ,(%)
    )
where

import Control.Lens
import Control.Arrow
import Data.Ratio
import GHC.Generics (Generic)
import Data.Traversable
import Data.Function
import Data.Foldable

-- | Note = pitch and duration.
data Note p d = Note { _pitch :: p, _dur :: d }
                deriving (Eq,Generic)
$(makeLenses ''Note)

instance (Show p, Show d) => Show (Note p d) where
    show (Note p d) = show p ++ "|:" ++ show d
instance Bifunctor Note where
    bimap f g (Note a b) = Note (f a) (g b)
instance Field1 (Note a b) (Note a' b) a a'
instance Field2 (Note a b) (Note a b') b b'

-- | Hand-rolled class providing monomorphic lenses.
class HasNote s p d | s -> p d where
  note :: Lens' s (Note p d)
  fromNote :: (HasNote n p d) => n -> s
  notePitch :: Lens' s p
  notePitch = note.pitch
  noteDur :: Lens' s d
  noteDur = note.dur
instance HasNote (Note p d) p d where
    note = ($)
    fromNote = view note

-- iso with pair
toPair :: Iso' (Note p d) (p,d)
toPair = iso (\(Note p d) -> (p,d)) (uncurry Note)


infixl 5 |:
-- | 'Note' smart constructor.
(|:) :: p -> d -> Note p d
(|:) = Note

-- | Monophonic pitch functor, i.e. Maybe with a sum monoid.
data Mono p = Rest | M { _mPitch :: p }
    deriving (Eq,Ord,Functor)
instance (Show p)=>Show (Mono p) where
    show Rest = "Rest"
    show (M p) = "M " ++ show p
makeLenses ''Mono
makePrisms ''Mono
instance Num p => Semigroup (Mono p) where
    Rest <> b = b
    a <> Rest = a
    (M a) <> (M b) = M (a + b)
instance Num p => Monoid (Mono p) where
    mempty = Rest
    mappend = (<>)

-- | Mono/Maybe isomorphism.
maybeMono :: Iso' (Maybe a) (Mono a)
maybeMono = iso toMono toMaybe
    where toMono Nothing = Rest
          toMono (Just a) = M a
          toMaybe Rest = Nothing
          toMaybe (M a) = Just a

-- | Mono 'HasNote'
mono :: HasNote n (Mono p) d => p -> d -> n
mono p = fromNote . mono' p

-- | Mono 'Note'.
mono' :: p -> d -> Note (Mono p) d
mono' p = Note (M p)

-- | Mono eliminator
unMono :: b -> (a -> b) -> Mono a -> b
unMono b _ Rest = b
unMono _ f (M a) = f a

-- | cf 'catMaybe'. Grab all non-rest values.
catMonos :: Foldable f => f (Mono a) -> [a]
catMonos = foldMap (unMono [] pure)

-- | 'Note' from duration, given 'Monoid' pitch.
-- Interoperates with 'chord' and 'mono'.
-- Useful for batch duration conversion.
rest :: (HasNote n p d, Monoid p) => d -> n
rest = fromNote . rest'

rest' :: Monoid p => d -> Note p d
rest' = Note mempty

isRest :: (Monoid p, Eq p, HasNote n p d) => n -> Bool
isRest = (mempty ==) . view notePitch




-- | Chroma as enharmonic names.
data Spelling = C|Cs|Db|D|Ds|Eb|E|F|Fs|Gb|G|Gs|Ab|A|As|Bb|B
            deriving (Eq,Show,Read,Enum,Ord,Bounded,Generic)

-- | Convert to 'Spelling' with 0==C, using 'Cs','Eb','Fs','Gs','Bb' enharmonics.
fromChroma :: Integral a => a -> Spelling
fromChroma 0 = C
fromChroma 1 = Cs
fromChroma 2 = D
fromChroma 3 = Eb
fromChroma 4 = E
fromChroma 5 = F
fromChroma 6 = Fs
fromChroma 7 = G
fromChroma 8 = Gs
fromChroma 9 = A
fromChroma 10 = Bb
fromChroma 11 = B
fromChroma n | n > 11 = fromChroma $ n `mod` 12
             | otherwise = fromChroma $ n `mod` 12 + 12

-- | 'Spelling' to 0-11.
toChroma :: Integral a => Spelling -> a
toChroma C = 0
toChroma Cs = 1
toChroma Db = 1
toChroma D = 2
toChroma Ds = 3
toChroma Eb = 3
toChroma E = 4
toChroma F = 5
toChroma Fs = 6
toChroma Gb = 6
toChroma G = 7
toChroma Gs = 8
toChroma Ab = 8
toChroma A = 9
toChroma As = 10
toChroma Bb = 10
toChroma B = 11

-- | 'Spelling'-to-chroma degenerate 'Iso'.
spelling :: Integral a => Iso' a Spelling
spelling = iso fromChroma toChroma

-- | Represent pitch as chroma and octave.
-- It's a full 'Num', 'Integral' instance, so negative octave values OK.
-- Instances use C4 == 60.
data PitchRep = PitchRep { _prPitch :: Spelling, _prOctave :: Int  }
              deriving (Eq,Bounded,Generic)
instance Show PitchRep where show (PitchRep s o) = show s ++ "@:" ++ show o
$(makeLenses ''PitchRep)

infixl 6 @:
(@:) :: Spelling -> Int -> PitchRep
(@:) = PitchRep

instance Num PitchRep where
    fromInteger i = fromChroma i @: ((fromIntegral i `div` 12) - 1)
    a * b = fromIntegral (toInteger a * toInteger b)
    a + b = fromIntegral (toInteger a + toInteger b)
    abs = fromIntegral . abs . toInteger
    signum = fromIntegral . signum . toInteger
    negate = fromIntegral . negate . toInteger

instance Enum PitchRep where
    toEnum = fromInteger . fromIntegral
    fromEnum = fromIntegral . toInteger

instance Ord PitchRep where
    a <= b = (fromIntegral a :: Integer) <= fromIntegral b

instance Real PitchRep where
    toRational (PitchRep s o) = (((fromIntegral o + 1) * 12) + toChroma s) % 1

instance Integral PitchRep where
    toInteger = truncate . toRational
    a `quotRem` b = (fromInteger *** fromInteger) (toInteger a `quotRem` toInteger b)

-- | Iso to integrals.
pitchRep :: Integral a => Iso' a PitchRep
pitchRep = iso fromIntegral (fromIntegral . toInteger)


--
-- Utilities
--

-- | compute total duration of notes
sumDurs :: (Num d, HasNote a p d, Traversable t) => t a -> d
sumDurs = sumOf (traverse.noteDur)

-- | map notes to arrival time
mapTime :: (Num d, Ord d, HasNote a p d, Traversable t) => t a -> [(d,a)]
mapTime = toList . snd .
          mapAccumL (\t n -> (t + view noteDur n,(t,n))) 0

-- | merge same-pitch notes
tied :: (Eq p,Num d,HasNote a p d,Traversable t,
          Traversable u,Snoc (u a) (u a) a a,Monoid (u a)) => t a -> u a
tied = merge ((==) `on` view notePitch)

tied' :: (Eq p,Num d,HasNote a p d,Traversable t) => t a -> [a]
tied' = tied

-- | merge rests with prior note
legato :: (Eq p,Monoid p,Num d,HasNote a p d,Traversable t,
          Traversable u,Snoc (u a) (u a) a a,Monoid (u a)) => t a -> u a
legato = merge $ \_ n -> view notePitch n == mempty

legato' :: (Eq p,Monoid p,Num d,HasNote a p d,Traversable t) => t a -> [a]
legato' = legato


-- | merge notes meeting some comparison by accumulating durations
merge :: (Num d,HasNote a p d,Traversable t,
          Traversable u,Snoc (u a) (u a) a a,Monoid (u a)) => (a -> a -> Bool) -> t a -> u a
merge cmp = acc mempty . toListOf traverse where
    acc rs [] = rs
    acc (rs :> r) (n:ns) | cmp r n = acc (rs |> over noteDur (+ view noteDur n) r) ns
    acc rs (n:ns) = acc (rs |> n) ns


-- | Pitch addition
transpose :: (Num p,HasNote a p d,Traversable t) => p -> t a -> t a
transpose by = over (traverse.notePitch) (+by)

-- | Pitch addition over a functor
transpose' :: (Num p,Functor f, HasNote a (f p) d,Traversable t) => p -> t a -> t a
transpose' by = over (traverse.notePitch.mapped) (+by)

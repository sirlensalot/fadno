{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Fadno.Xml
    ( -- * Score and Part
     xmlScore,xmlPart,xmlPartClef
    -- * Bars
    ,xmlMeasure
    ,xmlPrependMeasureData,xmlPrependMeasureDatas
    ,xmlAppendMeasureData,xmlAppendMeasureDatas
    ,xmlClef,xmlClef'
    ,xmlRepeats,xmlRepeats'
    ,xmlBarline,xmlBarline',xmlTimeSig,xmlRehearsalMark,xmlDirection
    -- * Notes
    ,xmlNote,xmlChord
    ,xmlTie
    -- * Rendering
    ,renderFile,renderString,renderElement,Element
    -- * Internals
    ,convertDurR,xmlDivisions
    ) where


import Fadno.MusicXml.MusicXml31
import Fadno.Xml.EmitXml
import qualified Data.Map.Strict as M
import qualified Fadno.Note as N
import qualified Fadno.Notation as N
import Data.List (mapAccumL)
import Data.Maybe
import GHC.Real
import Control.Arrow
import Text.XML.Light
import Data.String
import Control.Lens hiding (Empty)
import Data.Foldable
import Data.Monoid


makeClassy_ ''ChxMusicData
makeClassy_ ''Note
makeClassy_ ''ChxNote
makeClassy_ ''GrpFullNote
makeClassy_ ''MusicData
makeClassy_ ''Measure

makeClassy_ ''ScorePartwise
makeClassy_ ''ScoreHeader
makeClassy_ ''Identification
makeClassy_ ''PartList
makeClassy_ ''CmpPart



_testFile :: IO ()
_testFile = renderFile "output/newtest.xml" $
           xmlScore "Test" "Stoobie"
           [xmlPartClef "Hurdy Gurdy" "HGy" N.TrebleClef
            [xmlMeasure "1" $ xmlChord _testNote]]



-- | Hardcoded divisions.
xmlDivisions :: PositiveDivisions
xmlDivisions = 768

--
-- SCORE & PART
--

type MeasureList f = (Traversable f, Cons (f Measure) (f Measure) Measure Measure,
                      Snoc (f Measure) (f Measure) Measure Measure)

-- | Partwise score.
xmlScore :: String -> String -> [(CmpPart,ScorePart)] -> ScorePartwise
xmlScore title composer xmlParts =
    (mkScorePartwise
     ((mkScoreHeader doPartList)
      { scoreHeaderMovementTitle = Just title
      , scoreHeaderIdentification =
          Just (mkIdentification
                { identificationCreator =
                  [TypedText composer (Just "composer") ]}) })
        ) { scorePartwisePart = toListOf (traverse._1) xmlParts }
    where
      doPartList =
          PartList []
          (xmlParts ^?! _head._2)
          (map PartListScorePart (toListOf (_tail.traverse._2) xmlParts))


-- | Render partwise part and score parts.
xmlPart :: MeasureList f => String -> String -> f Measure -> (CmpPart,ScorePart)
xmlPart longName shortName measures =
    (CmpPart (fromString shortName) (toList $ addDivs measures),
     ScorePart
     (mkCmpScorePart (fromString shortName)
      (mkPartName (fromString longName)))
     { scorePartPartAbbreviation =
       Just (mkPartName (fromString shortName)) })
    where addDivs = xmlPrependMeasureData
                    (MusicDataAttributes
                      ((mkAttributes mkEditorial)
                       { attributesDivisions = Just xmlDivisions }))

-- | Render partwise part with clef.
xmlPartClef :: MeasureList f => String -> String -> N.Clef -> f Measure -> (CmpPart,ScorePart)
xmlPartClef l s c ms = xmlPart l s (xmlPrependMeasureData (xmlClef' c) ms)


--
-- BARS
--

type ApplyMonoid c t = (Applicative c,Monoid (c t))

-- | Partwise measure.
xmlMeasure :: Traversable t => String -> t ChxMusicData -> Measure
xmlMeasure mNumber = mkMeasure (fromString mNumber) . MusicData . toList

-- | Add datum to beginning of first measure
xmlPrependMeasureData :: (MeasureList f) => ChxMusicData -> f Measure -> f Measure
xmlPrependMeasureData = xmlPrependMeasureDatas . pure

-- | Add data to beginning of first measure
xmlPrependMeasureDatas :: (MeasureList f) => [ChxMusicData] -> f Measure -> f Measure
xmlPrependMeasureDatas d = over (_head._measureMusicData._musicDataMusicData) (d <>)

-- | Add datum to beginning of last measure
xmlAppendMeasureData :: (MeasureList f) => ChxMusicData -> f Measure -> f Measure
xmlAppendMeasureData = xmlAppendMeasureDatas . pure

-- | Add data to beginning of last measure
xmlAppendMeasureDatas :: (MeasureList f) => [ChxMusicData] -> f Measure -> f Measure
xmlAppendMeasureDatas d = over (_last._measureMusicData._musicDataMusicData) (d <>)


-- | Use a "Maybe Lens" to generate some or none of a datum.
maybeMusicDatas :: (ApplyMonoid c t) => Getting (Maybe a) s (Maybe a) -> (a -> c t) -> s -> c t
maybeMusicDatas l f = maybe mempty f . view l

-- | Use a "Maybe Lens" to generate one or none of a datum.
maybeMusicData :: (ApplyMonoid c t) => Getting (Maybe a) s (Maybe a) -> (a -> t) -> s -> c t
maybeMusicData l f = maybeMusicDatas l (pure.f)

-- | Clef in bar
xmlClef :: (ApplyMonoid c ChxMusicData, N.HasClef a) => a -> c ChxMusicData
xmlClef = maybeMusicData N.clef xmlClef'

-- | Clef alone.
xmlClef' :: N.Clef -> ChxMusicData
xmlClef' c =
    case c of
      N.TrebleClef -> mkC ClefSignG 2
      N.BassClef -> mkC ClefSignF 4
      N.AltoClef -> mkC ClefSignC 3
      N.PercClef -> mkC ClefSignPercussion 3
    where mkC cs cl =
              MusicDataAttributes
              ((mkAttributes mkEditorial)
               { attributesClef = [(mkClef cs)
                                   { clefLine = Just cl }]})


-- | Measure barlines.
xmlBarline :: (ApplyMonoid c ChxMusicData) => N.HasBarline a => a -> c ChxMusicData
xmlBarline = xmlBarline' False


-- | Measure barlines; flag determines if double bars are rendered to left (False)
-- or right (True).
xmlBarline' :: (ApplyMonoid c ChxMusicData) => Bool -> N.HasBarline a => a -> c ChxMusicData
xmlBarline' renderDoubleLeft = maybeMusicData N.barline $ \b ->
  case b of
    N.Double -> mdBarline doublePos
                BarStyleLightLight Nothing
    N.Final -> mdBarline RightLeftMiddleRight
               BarStyleLightHeavy Nothing
  where
    doublePos | renderDoubleLeft = RightLeftMiddleLeft
              | otherwise = RightLeftMiddleRight

-- | Measure repeats for a single measure.
xmlRepeats :: (ApplyMonoid t ChxMusicData) => N.HasRepeats a => a -> t ChxMusicData
xmlRepeats = maybeMusicDatas N.repeats $ \r ->
     case r of
        N.RStart -> pure startRepeat
        N.REnd -> pure endRepeat
        N.RBoth -> pure startRepeat <> pure endRepeat
    where

startRepeat :: ChxMusicData
startRepeat = mdBarline RightLeftMiddleLeft
              BarStyleHeavyLight (Just BackwardForwardForward)
endRepeat :: ChxMusicData
endRepeat = mdBarline RightLeftMiddleRight
            BarStyleLightHeavy (Just BackwardForwardBackward)

-- | Measure repeats bracketing existing measures.
xmlRepeats' :: (N.HasRepeats a, MeasureList f) => a -> f Measure -> f Measure
xmlRepeats' s measures =
    case view N.repeats s of
      Nothing -> measures
      Just N.RStart -> doStart measures
      Just N.REnd -> doEnd measures
      Just N.RBoth -> doStart . doEnd $ measures
    where doStart = xmlPrependMeasureData startRepeat
          doEnd = xmlAppendMeasureData endRepeat

-- | utility
mdBarline :: RightLeftMiddle -> BarStyle ->
             Maybe BackwardForward -> ChxMusicData
mdBarline rml bs bf =
    MusicDataBarline
    ((mkBarline mkEditorial)
     { barlineLocation = Just rml
     , barlineBarStyle = Just (mkBarStyleColor bs)
     , barlineRepeat = fmap mkRepeat bf })

-- | Measure time signature.
xmlTimeSig :: (ApplyMonoid t ChxMusicData, N.HasTimeSignature a) => a -> t ChxMusicData
xmlTimeSig = maybeMusicData N.timeSignature $ \(N.TimeSignature n q) ->
       MusicDataAttributes $
       (mkAttributes mkEditorial)
        { attributesTime =
          [mkTime (TimeTimeSignature
                   [ TimeSignature
                     (fromString $ show n)
                     (fromString $ show $ N.qToInt q)
                   ]
                   Nothing)]}

-- | Measure rehearsal mark.
xmlRehearsalMark :: (ApplyMonoid t ChxMusicData,N.HasRehearsalMark a) => a -> t ChxMusicData
xmlRehearsalMark = maybeMusicData N.rehearsalMark
               (makeDirection . DirectionTypeRehearsal . return .
                mkFormattedTextId . view N.rehearsalText)

-- | Measure direction.
xmlDirection :: (ApplyMonoid t ChxMusicData,N.HasDirection a) => a -> t ChxMusicData
xmlDirection = maybeMusicData N.direction
                   (makeDirection . DirectionTypeDirectionType . return . DirectionTypeWords .
                    mkFormattedTextId . view N.directionText)

-- | Utility for direction types
makeDirection :: ChxDirectionType -> ChxMusicData
makeDirection dt = MusicDataDirection
                        ((mkDirection mkEditorialVoiceDirection)
                         { directionDirectionType = [mkDirectionType dt] })



--
-- NOTES
--



-- | render note/rest as xml
xmlNote :: (N.HasNote a (N.Mono N.PitchRep) Rational) => a -> ChxMusicData
xmlNote n = MusicDataNote
            (mkNote (ChxNoteFullNote
                     (GrpFullNote Nothing
                      (fullNote (view N.notePitch n)))
                     (Duration durDivs) [])
             mkEditorialVoice)
            { noteType = Just (mkNoteType durNoteType)
            , noteDot = nds }
    where (durDivs,durNoteType,durDots) = convertDurR xmlDivisions $ view N.noteDur n
          nds = replicate durDots mkEmptyPlacement
          fullNote (N.M p) = FullNotePitch (convertPitchRep p)
          fullNote N.Rest = FullNoteRest mkRest

-- | render notes as xml chord or rest.
xmlChord :: (N.HasNote a [N.PitchRep] Rational) =>
            a -> [ChxMusicData]
xmlChord ch =
    case view N.notePitch ch of
      [] -> [doNote N.Rest]
      ps -> zipWith doChord [(0 :: Int)..] $ map (doNote.N.M) ps
    where doNote p = xmlNote (N.Note p (view N.noteDur ch))
          doChord i | i == 0 = id
                    | otherwise =
                        set (_musicDataNote._noteNote._chxnoteFullNote1._fullNoteChord)
                            (Just Empty)


_testNote :: N.Note' [N.PitchRep] Rational
_testNote = over N.nNote (view (bimapping (mapping N.pitchRep) (N.ratioPPQ N.PQ4))) N.testNote

-- | Adapt a rendered note to account for tie information.
-- > xmlTie testNote <$> xmlChord 128 testNote
xmlTie :: (N.HasTie a) => a -> ChxMusicData -> ChxMusicData
xmlTie a = over (_musicDataNote._noteNotations) (++adapt mkTNot) .
           over (_musicDataNote._noteNote._chxnoteTie) (++adapt' mkTie)
    where adapt fc = maybe [] (fmap fc . conv) $ view N.tie a
          conv N.TStart = [TiedTypeStart]
          conv N.TStop = [TiedTypeStop]
          conv N.TBoth = [TiedTypeStop,TiedTypeStart]
          adapt' fc = maybe [] (fmap fc . conv') $ view N.tie a
          conv' N.TStart = [StartStopStart]
          conv' N.TStop = [StartStopStop]
          conv' N.TBoth = [StartStopStop,StartStopStart]
          mkTNot s = (mkNotations mkEditorial)
                     {notationsNotations = [NotationsTied (mkTied s)]}

-- | Steps and enharmonics.
steps :: [(Step,Maybe Semitones)]
steps = [(StepC,Nothing),
         (StepC,sharp),
         (StepD,Nothing),
         (StepE,flat),
         (StepE,Nothing),
         (StepF,Nothing),
         (StepF,sharp),
         (StepG,Nothing),
         (StepA,flat),
         (StepA,Nothing),
         (StepB,flat),
         (StepB,Nothing)]
    where sharp = Just 1
          flat = Just (-1)

-- | Note values indexed by powers of two. [(1,Long) .. (1024,256th)]
noteTypeValues :: M.Map Int NoteTypeValue
noteTypeValues = M.fromList $ snd $ mapAccumL acc (256*4) [minBound .. maxBound]
    where acc v nt = (v `div` 2,(v,nt))

-- | Int pitch to xml. TODO C3 vs C4?
convertPitch :: Int -> Pitch
convertPitch i = Pitch step semi oct where
    oct = fromIntegral $ (i `div` 12) - 1
    (step, semi) = steps !! (i `mod` 12)

convertPitchRep :: N.PitchRep -> Pitch
convertPitchRep (N.PitchRep s o) = Pitch step semi (fromIntegral o)
    where (step,semi) = ss s
          sharp = Just 1
          flat = Just (-1)
          ss N.C = (StepC,Nothing)
          ss N.Cs = (StepC,sharp)
          ss N.Db = (StepD,flat)
          ss N.D = (StepD,Nothing)
          ss N.Ds = (StepD,sharp)
          ss N.Eb = (StepE,flat)
          ss N.E = (StepE,Nothing)
          ss N.F = (StepF,Nothing)
          ss N.Fs = (StepF,sharp)
          ss N.Gb = (StepG,flat)
          ss N.G = (StepG,Nothing)
          ss N.Gs = (StepG,sharp)
          ss N.Ab = (StepA,flat)
          ss N.A = (StepA,Nothing)
          ss N.As = (StepA,sharp)
          ss N.Bb = (StepB,flat)
          ss N.B = (StepB,Nothing)


-- | Int duration/PPQ to xml values.
convertDur :: N.PPQ -> Int -> PositiveDivisions -> (PositiveDivisions,NoteTypeValue,Int)
convertDur ppq dur xdivs = (fromIntegral divs,findValue,dots)
    where
      ppqd = N.ppqDiv ppq
      divs = floor xdivs * dur `div` ppqd
      (num,denom) = numerator &&& denominator $ (dur % (ppqd * 16))
      dots = fromMaybe 0 $ M.lookup num dotValues
      findValue = fromMaybe NoteTypeValue256th $
                  M.lookup (denom `div` (2 ^ dots))  noteTypeValues

-- | Rational duration (ie, '1 % 4' for quarter note) to xml values.
convertDurR :: PositiveDivisions -> Rational -> (PositiveDivisions,NoteTypeValue,Int)
convertDurR xdivs r' = (fromIntegral divs,findValue,dots)
    where
      r = reduce (numerator r') (denominator r')
      divs :: Int
      divs = floor $ toRational xdivs * (r * 4)
      (num,denom) = numerator &&& denominator $ r
      dots = fromMaybe 0 $ M.lookup (fromIntegral num) dotValues
      findValue = fromMaybe NoteTypeValue256th $
                  M.lookup (fromIntegral denom `div` (2 ^ dots)) noteTypeValues

-- | Numerator values to dots.
dotValues :: M.Map Int Int
dotValues = M.fromList $ takeWhile (<= 1024) (dot 3 4) `zip` [1..]
    where dot v i = v:dot (v + i) (i * 2)

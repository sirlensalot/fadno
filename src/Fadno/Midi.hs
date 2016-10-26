{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}


module Fadno.Midi where

import Sound.MIDI.File as MFile
import Data.EventList.Relative.TimeBody as EList hiding (concat,traverse)
import Sound.MIDI.File.Event as MEvent
import Sound.MIDI.File.Event.Meta as MMeta
import Sound.MIDI.Message.Channel as MChan
import Sound.MIDI.Message.Channel.Voice as MVoice
import Sound.MIDI.File.Load
import Sound.MIDI.File.Save
import Sound.MIDI.General
import Fadno.Note
import Data.List (mapAccumL)
import Control.Lens
import Control.Arrow
import System.Process
import Control.Monad
import Data.Ratio

-- | Serializable midi data.
type MidiData = MFile.T

type IPitch = Int
type IDur = Int

-- | Convert some note value to midi-ready values.
class MidiNotes a where
    toMidiNotes :: a -> [([IPitch],IDur)]

instance MidiNotes [([IPitch],IDur)] where toMidiNotes = id

instance {-# OVERLAPPING #-} (Integral p, Traversable c, Integral d, Traversable t) => MidiNotes (t (Note (c p) d))  where
    toMidiNotes = map ((map fromIntegral . toListOf traverse) *** fromIntegral) .
                  toListOf (traverse.toPair)

instance (Integral p, Integral d, Traversable t) => MidiNotes (t (Note p d))  where
    toMidiNotes = map (return.fromIntegral *** fromIntegral) . toListOf (traverse.toPair)


-- | Tempo in microseconds per quarter. See 'fromBPM'.
newtype MidiTempo = MidiTempo Int
    deriving (Eq,Show,Enum,Bounded,Ord,Num,Real,Integral)

-- | Midi channel, 1-16 presumably.
newtype MidiChan = MidiChan Int
    deriving (Eq,Show,Enum,Bounded,Ord,Num,Real,Integral)

-- | note velocity, 0-127
newtype MidiVelocity = MidiVelocity Int
    deriving (Eq,Show,Enum,Bounded,Ord,Num,Real,Integral)

-- | Midi program. See 'fromInstrument'.
newtype MidiProgram = MidiProgram Int
    deriving (Eq,Show,Enum,Bounded,Ord,Num,Real,Integral)

-- | Midi ticks per quarter.
newtype MidiTicks = MidiTicks Int
    deriving (Eq,Show,Enum,Bounded,Ord,Num,Real,Integral)

-- | Rational to ticks
toTicks :: MidiTicks -> Iso' Rational IDur
toTicks t = iso to' from' where
    to' = truncate . (* fromIntegral (t*4))
    from' = (% fromIntegral (t*4)) . fromIntegral


-- | Internal type for midi event or pad.
data MidiEvent = Pad IDur | Event MEvent.T

-- | cover our tracks
type MidiTrack = Track


-- | write to disk.
writeMidiFile :: FilePath -> MidiData -> IO ()
writeMidiFile = toFile

-- | debug midi file.
showMidiFile :: FilePath -> IO ()
showMidiFile = showFile

-- | Make midi file data
midi :: MidiTicks -> [MidiTrack] -> MidiData
midi ticks = MFile.Cons Parallel (Ticks (toTempo $ fromIntegral ticks))

-- | make a standard track which specifies tempo and program.
-- | see 'makeTrack' for more control.
makeTrackFull
  :: (MidiNotes notes) =>
     MidiTempo
     -> MidiChan
     -> MidiProgram
     -> MidiVelocity
     -> notes
     -> MidiTrack
makeTrackFull tempo chan prog vel notes =
    makeTrack $ setTempo tempo:
                programChange chan prog:
                toNoteEvents chan vel notes


-- | BPM to microseconds per quarter note.
fromBPM :: (Real a, Show a) => a -> MidiTempo
fromBPM b | b > 0 = floor (60 * 1000000 / toRational b)
          | otherwise = error $ "fromBPM: must be > 0: " ++ show b

-- | convert a General MIDI 'Instrument'.
fromInstrument :: Instrument -> MidiProgram
fromInstrument = fromIntegral . fromEnum

-- | make a track from track events.
makeTrack :: [MidiEvent] -> MidiTrack
makeTrack = fromPairList . concat . snd . mapAccumL conv 0
    where conv :: IDur -> MidiEvent -> (IDur,[(ElapsedTime,MEvent.T)])
          conv _ (Pad dur) = (dur,[])
          conv off (Event e) = (0,[(toElapsedTime $ fromIntegral off,e)])


-- | turn notes into track events.
toNoteEvents :: MidiNotes notes => MidiChan -> MidiVelocity -> notes -> [MidiEvent]
toNoteEvents chan vel = concatMap (noteEvents chan vel) . toMidiNotes


-- | create a "Voice" MIDI event
voiceEvent :: MidiChan -> MVoice.T -> MidiEvent
voiceEvent chan = midiEvent chan . Voice

-- | tempo meta event.
setTempo :: MidiTempo -> MidiEvent
setTempo = metaEvent . SetTempo . toTempo . fromIntegral

-- | create a "Meta" MIDI event
metaEvent :: MMeta.T -> MidiEvent
metaEvent = Event . MetaEvent

-- | create a "Voice" or "Mode" MIDI event.
midiEvent :: MidiChan -> MChan.Body -> MidiEvent
midiEvent chan = Event . MIDIEvent . MChan.Cons (toChannel $ fromIntegral chan)

-- TODO: sysex.

-- | program change MIDI Voice event.
programChange :: MidiChan -> MidiProgram -> MidiEvent
programChange chan prog = voiceEvent chan (ProgramChange (toProgram $ fromIntegral prog))

-- | note on + note off events, using 'Pad' to carve out space.
noteEvents :: MidiChan -> MidiVelocity -> ([IPitch],IDur) -> [MidiEvent]
noteEvents chan vel (ps,dur) = evs noteOn ++ [Pad dur] ++ evs noteOff
    where evs f = map (f chan vel . fromIntegral) ps

-- TODO: figure out polymorphic way to attach velocity and anything else to notes.

-- | note on or note off event.
noteEvent :: (Pitch -> Velocity -> MVoice.T) ->
             MidiChan -> MidiVelocity -> IPitch -> MidiEvent
noteEvent f chan vel pitch = voiceEvent chan
                             (f (toPitch (fromIntegral pitch))
                                    (toVelocity $ fromIntegral vel))
noteOn :: MidiChan -> MidiVelocity -> IPitch -> MidiEvent
noteOn = noteEvent NoteOn

noteOff :: MidiChan -> MidiVelocity -> IPitch -> MidiEvent
noteOff = noteEvent NoteOff


test1 :: IO ()
test1 = playMidi "/tmp/first.midi" 120 [(AcousticGrandPiano,
         map (uncurry Note)
                 [([60 :: Int],48 :: Int),([61],48),([62],24),([64],64),
                  ([],96),([60,66],96)])]


playMidi :: MidiNotes n => FilePath -> Int -> [(Instrument,n)] -> IO ()
playMidi file bpm tracks = do
    writeMidiFile file $ midi 96 $ map (\(inst,notes) -> makeTrackFull (fromBPM bpm) 0 (fromInstrument inst) 127 notes) tracks
    void $ createProcess (shell $ "scripts/qt7play.applescript " ++ file)



-- playMidi "/tmp/boston.mid" DrawbarOrgan notes
-- let boston = [Db@:5,F@:4,Db@:5,Eb@:5,Ab@:4,C@:5]
-- map (\p -> (p - 60) * 2 + 60)
-- let notes = concat $ replicate 8 $ map (`Note` (1 % 16)) boston
-- playMidi "/tmp/boston.mid" DrawbarOrgan 140
--    (toListOf (traverse.seconding (toTicks 96)) notes)

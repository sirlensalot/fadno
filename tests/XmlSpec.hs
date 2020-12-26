
module XmlSpec where

import Test.HUnit
import Fadno.Meter
import Data.Ratio
import Fadno.Note
import Fadno.Notation
import Control.Lens
import GHC.Real
import Fadno.Xml
import Fadno.MusicXml.MusicXml31

tests :: Test
tests = "xml" ~: [
  "convertDurR" ~:
    [ convertDurR xmlDivisions (1 % 4) @?= (xmlDivisions,NoteTypeValueQuarter,0)
    , convertDurR xmlDivisions (1 % 2) @?= (xmlDivisions*2,NoteTypeValueHalf,0)
    , convertDurR xmlDivisions (2 :% 4) @?= (xmlDivisions*2,NoteTypeValueHalf,0)
    , convertDurR xmlDivisions (3 % 4) @?= (xmlDivisions*3,NoteTypeValueHalf,1)
    ]
  ]

module Main where

import qualified MeterSpec
import qualified NotationSpec
import qualified NoteSpec
import qualified UtilSpec
import qualified XmlSpec
import Test.HUnit
import System.IO
import Control.Monad
import Test.Hspec
import Test.Hspec.Contrib.HUnit

main = hspec $ fromHUnitTest $ TestList
           [MeterSpec.tests,
            NotationSpec.tests,
            NoteSpec.tests,
            UtilSpec.tests,
            XmlSpec.tests]

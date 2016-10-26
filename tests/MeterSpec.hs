
module MeterSpec where

import Test.HUnit
import Fadno.Meter
import Data.Ratio
import Fadno.Note
import Fadno.Notation
import Control.Lens
import GHC.Real

_i :: Int -> Int
_i = id
_n = map (map (_i 1|:))


tests :: Test
tests = "meter" ~: [
         "rebar" ~: [
          rebars' (TimeSignature 4 Q4)
          [[[_i 1]|:2%8,rest $ 3%8,[1,2]|:11%8] ,
           [[4,5]|:4%8,rest $ 5%8],
           [[3]|:3%8]]
          @?=
          [bar [note' ([1]|:2%8),rest (3%8),note' ([1,2]|:3%8) & tie ?~ TStart]
                   & timeSignature ?~ 4/:Q4 ,
           bar [note' ([1,2]|:8%8) & tie ?~ TStop] ,
           bar [note' ([4,5]|:4%8), rest (4%8)] ,
           bar [rest (1%8),note' ([3]|:3%8)] & timeSignature ?~ 2/:Q4],
          rebars' (TimeSignature 4 Q4)
          [[[_i 1]|:2%8,rest $ 3%8,[1,2]|:11%8],
           [[4,5]|:4%8,rest $ 5%8],
           [[3]|:1%8]]
          @?=
          [bar [note' ([1]|:2%8),rest $ 3%8,note' ([1,2]|:3%8) & tie ?~ TStart]
                   & timeSignature ?~ 4/:Q4,
           bar [note' ([1,2]|:8%8) & tie ?~ TStop],
           bar [note' ([4,5]|:4%8), note' ([]|:1 % 2),note' ([]|:1 % 8),note' ([3]|:1 % 8)]
                   & timeSignature ?~ 5/:Q4],
          rebar' (TimeSignature 5 Q4)
          [([_i 40]|:2%8),([41]|:2%8),([42]|:1%8),
           ([41]|:5%8),([42]|:3%8),([41]|:2%8)]
          @?=
          [bar [note' ([40]|:2%8),note' ([41]|:2%8),note' ([42]|:1%8),
                   note' ([41]|:5%8)] & timeSignature ?~ (5/:Q4),
           bar [note' ([42]|:3%8),note' ([41]|:2%8)] & timeSignature ?~ (5/:Q8)],
          rebar (TimeSignature 5 Q4)
          [([_i 40]|:2%8),([41]|:2%8),([42]|:1%8),
                           ([41]|:5%8),([42]|:3%8)] @?=
          [bar [note' ([40]|:2%8),note' ([41]|:2%8),note' ([42]|:1%8),note' ([41]|:5%8),
                note' ([42]|:3%8)] & timeSignature ?~ (13/:Q8)],
          rebar' (2/:Q4) [[_i 1]|:4%1] @?=
          [bar [note' ([1]|:1 % 2) & tie ?~ TStart] & timeSignature ?~ 2/:Q4,
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TBoth],
           bar [note' ([1]|:1 % 2) & tie ?~ TStop]],
          -- Bad TS bug
          rebar (4/:Q4) [M 72|:11 % 8] @?=
          [bar [note' (M 72|:1 % 1) & tie ?~ TStart,note' (M 72|:3 % 8) & tie ?~ TStop] & timeSignature ?~ 11/:Q8],
          -- bad joinLast bug
          rebar (4/:Q4) [M 72|:9 % 8,M 72|:1%4] @?=
          [bar [note' (M 72|:1 % 1) & tie ?~ TStart,note' (M 72|:1 % 8) & tie ?~ TStop,
                note' (M 72|:1 % 4)] & timeSignature ?~ 11/:Q8]


         ],
        "validDur" ~: [
         filter ((==False).snd) (map (\d -> (d,validDur d))
                                 [2,
                                  1,3%2,7%4,
                                  1%2,3%4,7%8,
                                  1%8,3%16,
                                  1%5,2%5,3%5,7%5,
                                 1%3,2%3,4%3]) @?= [],
         filter ((==True).snd) (map (\d -> (d,validDur d))
                                [3,7%2,15%4,15%8,
                                 5%4,9%8,15%16,
                                 15%5,5%3,15%7]) @?= []
        ],
         "splitDur" ~: [
          splitDur (17%20) @?= [3 % 5,1 % 4],
          splitDur (5:%10) @?= [1 % 5,3 % 10],
          splitDur 4 @?= [2,2],
          splitDur (5%8) @?= [1 % 4,3 % 8]
         ],
         "tieRules" ~: [
          -- test normal split
          tieRules' [mono' (_i 1) $ 5%8,rest $ 5%8] @?=
          bar [mono 1 (1%4) & tie ?~ TStart,
               mono 1 (3%8) & tie ?~ TStop,
               rest (1%4),
               rest (3%8)],
          -- test split with pre-existing tie (both)
          tieRules' [note' ([_i 1]|:5%8) & tie ?~ TBoth] @?=
          bar [note' ([1]|:1%4) & tie ?~ TBoth,
               note' ([1]|:3%8) & tie ?~ TBoth],
          -- test split with pre-existing tie (start)
          tieRules' [note' ([_i 1]|:5%8) & tie ?~ TStart] @?=
          bar [note' ([1]|:1%4) & tie ?~ TStart,
               note' ([1]|:3%8) & tie ?~ TBoth],
          -- test split with pre-existing tie (end)
          tieRules' [note' ([_i 1]|:5%8) & tie ?~ TStop] @?=
          bar [note' ([1]|:1%4) & tie ?~ TBoth,
               note' ([1]|:3%8) & tie ?~ TStop],
          -- test no split, with pre-existing tie
          tieRules' [note' ([_i 1]|:1) & tie ?~ TBoth] @?= bar [note' ([_i 1]|:1) & tie ?~ TBoth],
          -- test split, one note
          tieRules' [[_i 1]|:11%8] @?=
          bar [note' ([1]|:2:%4) & tie ?~ TStart,
               note' ([1]|:7%8) & tie ?~ TStop]
         ],
         "selectTimeSig" ~: [
          selectTimeSig (_n [[3%4,2%4,3%8,2%8],[3%4,2%4]])
                            @?= Just (5/:Q4),
          selectTimeSig (_n [[1%4,1%4,1%4,1%4],[2%4,2%4,1%4,1%4,2%4]])
                            @?= Just (4/:Q4),
          selectTimeSig (_n [[3%8,3%8,3%8,3%8],[1%8,1%4,3%8,3%4],[3%8,3%8]])
                            @?= Just (6/:Q8)
         ]
        ]

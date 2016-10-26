
module NotationSpec where

import Test.HUnit
import Fadno.Notation
import Data.Ratio

tests = "notation" ~: [
         "qFromInt" ~: [
          map qFromInt [0::Int,1,2,3,4,5,8,16,32,64] @?=
          [Nothing,Nothing,Just Q2,Nothing,Just Q4,Nothing,
           Just Q8,Just Q16,Just Q32,Just Q64],
          map qToInt [Q2,Q4,Q8,Q16,Q32,Q64] @?=
          [2,4,8,16,32,64]
         ],
         "tsFromRatio" ~: [
          tsFromRatio (1%4) @?= Just (2/:Q8) -- "1" nums become 2
         ,tsFromRatio (2%1) @?= Just (8/:Q4) -- "1" denoms become 4
         ,tsFromRatio (3%8) @?= Just (3/:Q8)
         ,tsFromRatio (3%5) @?= Nothing
         ]
        ]

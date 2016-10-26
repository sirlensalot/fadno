
module UtilSpec where

import Test.HUnit
import Fadno.Util
import qualified Data.Map as M



tests = "util" ~: [
         "diff" ~: test [
                     diff [5,10,8,2,3] @?= [5,-2,-6,1]
                    ],
         "normalize" ~: test [
                      normalize [1,3,2] @?= [0,2,1]
                     ],
         "pitchClassSet" ~: test [
                              pitchClassSet 12 [1,2,3,5,9,13,16] @?= [0,1,2,3,4,8],
                              pitchClassSet 12 [4,6,2,11] @?= [0,3,5,7],
                              pitchClassSet 12 [19,9,10,5,8,0] @?= [0,2,3,4,5,7]
         ], "monotonic" ~: test [
          monotonic [1,2,4] @?= True,
          monotonic [4,4,5,9] @?= True,
          monotonic [4,4,3] @?= True,
          monotonic [-1,-3,-5] @?= True,
          monotonic [1,2,1] @?= False,
          monotonic [2,1,1,3] @?= False
         ], "interleave" ~: test [
          interleave [[1,2,3,4],[5,6,7],[8,9,0]] @?= [1,5,8,2,6,9,3,7,0]
         ], "pivot" ~: test [
          pivot [[1,2,3,4],[5,6,7],[8,9,0]] @?= [[1,5,8],[2,6,9],[3,7,0]]
         ], "lfsr" ~: test [
          take 30 (lfsr 4 2 3) @?=
               take 30 (cycle [False,False,False,True,False,False,True,True,
                                    False,True,False,True,True,True,True])
         ], "filterOnKeys" ~: test [
          filterOnKeys [1,5] (M.fromList [(1,2),(3,4),(5,6),(7,8)]) @?=
                       M.fromList [(1,2),(5,6)]
         ], "pairBy" ~: test [
          pairBy (+2) [1,2,3] @?=  [(1,3),(2,4),(3,5)]
         ], "delim" ~: test [
          delim "," ["1","2","3","4"] @?= "1,2,3,4"
         ]
        ]

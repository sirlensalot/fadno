
module NoteSpec where

import Test.HUnit
import Fadno.Note


_m :: Note (Mono Int) Int -> Note (Mono Int) Int; _m = id
_i :: Note Int Int -> Note Int Int; _i = id
_c :: Note [Int] Int -> Note [Int] Int; _c = id
tests :: Test
tests = "domain" ~: [
         "tied" ~: [
          tied [_i$2|:1,2|:2] @?= [2|:3],
          tied [_c$[2,3]|:1,[2,3]|:2] @?= [[2,3]|:3],
          tied ([_m$M 2|:1,rest 1]) @?= [M 2|:1,rest 1],
          --tied ([[3]|:2,[3,2]|:3,[3,2]|:2,[3]|:5,[3]|:3]) @?=
          --        [[3]|:2,[3,2]|:5,[3]:8],
          tied ([_i$2|:1]) @?= [2|:1],
          tied ([_m(rest 1),rest 2,M 2|:4]) @?= [rest 3,M 2|:4]
          ],
         "legato" ~: [
          legato [_m$M 2|:2,rest 2,M 3|:4] @?= [M 2|:4,M 3|:4],
          legato [_c(rest 2),[3,4]|:4,rest 5,[6]|:7] @?= [rest 2,[3,4]|:9,[6]|:7]
                 ],
         "sumDurs" ~: [
          sumDurs [_c$[1]|:1,rest 2] @?= 3,
          sumDurs ([] :: [Note Int Int]) @?= 0
         ],
         "mapTime" ~: [
          mapTime [_i$1|:2,3|:4,5|:6] @?= [(0,1|:2),(2,3|:4),(6,5|:6)]
         ]
        ]

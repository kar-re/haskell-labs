module Main where

import StringAlignment
import Test.HUnit

scoreMatch = 0

scoreMismatch = -1

scoreSpace = -1

string1 = "writers"

string2 = "vintner"

similarityScoreTest = test [similarityScore string1 string2 ~?= -5]

optAlignmentsTest = test [optAlignments string1 string2 ~?= [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]


main =
  runTestTT $
    test
      [ "similarityScore" ~: Main.similarityScoreTest,
        "optAlignments" ~: Main.optAlignmentsTest,
      ]

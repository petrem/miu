module LibSpec where

import Data.Either       (isLeft, fromRight)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib
  ( isSegmentOf
  , getSegment
  , replaceSegment
  , findSubstrings
  , replaceSubstrings
  )

-- main :: IO ()
-- main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec = do
  describe "isSegmentOf" $ for_ isSegmentOfCases isSegmentOfTest
  describe "getSegment" $ for_ getSegmentCases getSegmentTest
  describe "replaceSegment" $ for_ replaceSegmentCases replaceSegmentTest
  describe "findSubstrings" $ for_ findSubstringsCases findSubstringsTest
  describe "replaceSubstrings" $ for_ replaceSubstringsCases replaceSubstringsTest
  where
    isSegmentOfTest (l, pos, len, xs, expected) = it description assertion
      where
        description = l ++ " pos: " ++ show pos ++ " len: " ++ show len ++ " in: " ++ show xs
        assertion = isSegmentOf pos len xs `shouldSatisfy` expected

    isSegmentOfCases :: [(String, Int, Int, [Int], (Either a Bool) -> Bool)]
    isSegmentOfCases =
      [ ("negative position", -1, 1, [], isLeft)
      , ("negative position", -1, 0, [], isLeft)
      , ("zero-length segment", 0, 0, [], matchesRight True)
      , ("zero-length segment", 1, 0, [], matchesRight True)
      , ("zero-length segment", 0, 0, [1,2,3], matchesRight True)
      , ("zero-length segment", 1, 0, [1,2,3], matchesRight True)
      , ("zero-length segment", 2, 0, [1,2,3], matchesRight True)
      , ("zero-length segment", 3, 0, [1,2,3], matchesRight True)
      , ("empty list", 0, 1, [], matchesRight False)
      , ("empty list", 1, 1, [], matchesRight False)
      , ("inside list", 0, 1, [1,2,3], matchesRight True)
      , ("inside list", 1, 1, [1,2,3], matchesRight True)
      , ("inside list", 0, 2, [1,2,3], matchesRight True)
      , ("inside list", 1, 2, [1,2,3], matchesRight True)
      , ("entire list", 0, 3, [1,2,3], matchesRight True)
      , ("overlapping", 0, 4, [1,2,3], matchesRight False)
      , ("overlapping", 1, 3, [1,2,3], matchesRight False)
      , ("overlapping", 2, 2, [1,2,3], matchesRight False)
      , ("after", 3, 1, [1,2,3], matchesRight False)
      ]

    getSegmentTest (l, pos, len, xs, expected) = it description assertion
      where
        description = l ++ " pos: " ++ show pos ++ " len: " ++ show len ++ " in: " ++ show xs
        assertion = getSegment pos len xs `shouldSatisfy` expected

    getSegmentCases :: [(String, Int, Int, [Int], (Either a [Int]) -> Bool)]
    getSegmentCases =
      [ ("negative position", -1, 1, [], isLeft)
      , ("negative position", -1, 0, [], isLeft)
      , ("zero-length segment", 0, 0, [], matchesRight [])
      , ("zero-length segment", 1, 0, [], matchesRight [])
      , ("zero-length segment", 0, 0, [1,2,3], matchesRight [])
      , ("zero-length segment", 1, 0, [1,2,3], matchesRight [])
      , ("zero-length segment", 2, 0, [1,2,3], matchesRight [])
      , ("zero-length segment", 3, 0, [1,2,3], matchesRight [])
      , ("empty list", 0, 1, [], isLeft)
      , ("empty list", 1, 1, [], isLeft)
      , ("inside list", 0, 1, [1,2,3], matchesRight [1])
      , ("inside list", 1, 1, [1,2,3], matchesRight [2])
      , ("inside list", 0, 2, [1,2,3], matchesRight [1,2])
      , ("inside list", 1, 2, [1,2,3], matchesRight [2,3])
      , ("entire list", 0, 3, [1,2,3], matchesRight [1,2,3])
      , ("overlapping", 0, 4, [1,2,3], isLeft)
      , ("overlapping", 1, 3, [1,2,3], isLeft)
      , ("overlapping", 2, 2, [1,2,3], isLeft)
      , ("after", 3, 1, [1,2,3], isLeft)
      ]

    replaceSegmentTest (l, pos, len, xs, ys, expected) = it description assertion
      where
        description = l ++ " pos: " ++ show pos ++ " len: " ++ show len ++ " in: " ++ show xs ++ " with " ++ show ys
        assertion = replaceSegment pos len xs ys `shouldSatisfy` expected

    replaceSegmentCases :: [(String, Int, Int, [Int], [Int], (Either a [Int]) -> Bool)]
    replaceSegmentCases =
      [ ("negative position", -1, 1, [], [1], isLeft)
      , ("negative position", -1, 0, [], [1], isLeft)
      , ("empty segment (insert)", 0, 0, [], [1], matchesRight [1])
      , ("empty segment (insert)", 1, 0, [], [1], matchesRight [1]) -- TODO: do I want this behaviour?
      , ("empty segment (insert)", 0, 0, [1,2,3], [4], matchesRight [4,1,2,3])
      , ("empty segment (insert)", 1, 0, [1,2,3], [4], matchesRight [1,4,2,3])
      , ("empty segment (insert)", 2, 0, [1,2,3], [4], matchesRight [1,2,4,3])
      , ("empty segment (insert) outside list", 3, 0, [1,2,3], [4], matchesRight [1,2,3,4])
      , ("non-empty segment in empty list", 0, 1, [], [1], isLeft)
      , ("non-empty segment in empty list", 1, 1, [], [1], isLeft)
      , ("replace one element inside list", 0, 1, [1,2,3], [4], matchesRight [4,2,3])
      , ("replace one element inside list", 1, 1, [1,2,3], [4],  matchesRight [1,4,3])
      , ("replace one element inside list", 2, 1, [1,2,3], [4],  matchesRight [1,2,4])
      , ("replace elements inside list", 0, 2, [1,2,3], [4], matchesRight [4,3])
      , ("replace elements inside list", 0, 2, [1,2,3], [4,5], matchesRight [4,5,3])
      , ("replace elements inside list", 1, 2, [1,2,3], [4], matchesRight [1,4])
      , ("replace elements inside list", 1, 2, [1,2,3], [4,5], matchesRight [1,4,5])
      , ("overlapping", 0, 4, [1,2,3], [4], isLeft)
      , ("overlapping", 1, 3, [1,2,3], [4], isLeft)
      , ("overlapping", 2, 2, [1,2,3], [4], isLeft)
      , ("after", 3, 1, [1,2,3], [4], isLeft)
      ]

    findSubstringsTest (l, needle, haystack, expected) = it description assertion
      where
        description = l ++ ": '" ++ needle ++ "' in '" ++ haystack ++ "'"
        assertion = findSubstrings needle haystack `shouldBe` expected

    findSubstringsCases =
      [ ("empty haystack", "", "", [0])
      , ("empty haystack", "abc", "", [])
      , ("empty needle", "", "a", [0,1])
      , ("empty needle", "", "abc", [0,1,2,3])
      , ("single match", "abc", "abc", [0])
      , ("single match", "abc", "abcd", [0])
      , ("single match", "abc", "zabc", [1])
      , ("single match", "abc", "zabcd", [1])
      , ("multiple matches", "abc", "abcabc", [0,3])
      , ("multiple matches", "abc", "abcxabc", [0,4])
      , ("multiple matches", "abc", "zabcabcd", [1,4])
      , ("multiple overlapped matches", "aba", "abababa", [0,2,4])
      , ("no matches", "abc", "ab", [])
      , ("no matches", "abc", "abbc", [])
      , ("no matches", "abc", "xbcab", [])
      ]

    replaceSubstringsTest (l, needle, replacement, haystack, expected) = it description assertion
      where
        description = l ++ ": '" ++ needle ++ "' with '" ++ replacement ++ "' in '" ++ haystack ++ "'"
        assertion = replaceSubstrings needle replacement haystack `shouldBe` expected

    replaceSubstringsCases =
      [ ("empty haystack", "", "", "", [""])
      , ("empty haystack", "abc", "x", "", [])
      , ("empty needle", "", "x", "a", ["xa", "ax"])
      , ("empty needle", "", "xy", "abc", ["xyabc", "axybc", "abxyc", "abcxy"])
      , ("single match", "abc", "x", "abc", ["x"])
      , ("single match", "abc", "xy", "abcd", ["xyd"])
      , ("single match", "abc", "x", "zabc", ["zx"])
      , ("single match", "abc", "x", "zabcd", ["zxd"])
      , ("multiple matches", "abc", "x", "abcabc", ["xabc", "abcx"])
      , ("multiple matches", "abc", "x", "abczabc", ["xzabc", "abczx"])
      , ("multiple matches", "abc", "x", "zabcabcd", ["zxabcd", "zabcxd"])
      , ("multiple overlapped matches", "aba", "x", "abababa", ["xbaba", "abxba", "ababx"])
      , ("single match with empty string", "abc", "", "abc", [""])
      , ("single match with empty string", "abc", "", "abcd", ["d"])
      , ("single match with empty string", "abc", "", "zabc", ["z"])
      , ("single match with empty string", "abc", "", "zabcd", ["zd"])
      , ("no matches", "abc", "x", "ab", [])
      , ("no matches", "abc", "x", "abbc", [])
      , ("no matches", "abc", "x", "xbcab", [])
      ]

matchesRight :: (Eq b) => b -> Either a b -> Bool
matchesRight x = (fromRight False) . (fmap (== x))

-- TODO:
-- - use text formatting for descriptions
-- - how to show expected values?
-- - format code

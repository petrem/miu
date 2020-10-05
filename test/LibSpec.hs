module LibSpec where

import Data.Either       (isLeft, fromRight)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib
  ( isSegmentOf
  , getSegment
  , replaceSegment
  )

-- main :: IO ()
-- main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec = do
  describe "isSegmentOf" $ for_ isSegmentOfCases isSegmentOfTest
  describe "getSegment" $ for_ getSegmentCases getSegmentTest
  describe "replaceSegment" $ for_ replaceSegmentCases replaceSegmentTest

  where
    isSegmentOfTest (l, pos, len, xs, expected) = it description assertion
      where
        description = l ++ " pos: " ++ show pos ++ " len: " ++ show len ++ " in: " ++ show xs
        assertion = (isSegmentOf pos len xs) `shouldSatisfy` expected

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
        assertion = (getSegment pos len xs) `shouldSatisfy` expected

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
        description = l ++ "pos: " ++ show pos ++ " len: " ++ show len ++ " in: " ++ show xs ++ " with " ++ show ys
        assertion = (replaceSegment pos len xs ys) `shouldSatisfy` expected

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

matchesRight :: (Eq b) => b -> Either a b -> Bool
matchesRight x = (fromRight False) . (fmap (== x))

-- TODO:
-- - use text formatting for descriptions
-- - how to show expected values?
-- - format code

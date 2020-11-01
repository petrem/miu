module Lib
  ( getSegment
  , replaceSegment
  , isSegmentOf
  , findSubstrings
  , replaceSubstrings
  ) where

import Data.Either
import Data.List (findIndices, isPrefixOf, tails)


-- get segment defined by position and length
getSegment :: Int -> Int -> [a] -> Either String [a]
getSegment pos len xs
  | fromRight False $ isSegmentOf pos len xs = Right $ take len . drop pos $ xs
  | otherwise = Left $ "Segment " ++ show (pos,len) ++ " outside list of length " ++ show (length xs)

--replace a segment defined by position and length with a different list
replaceSegment :: Int -> Int -> [a] -> [a] -> Either String [a]
replaceSegment pos len xs ys
  | fromRight False $ isSegmentOf pos len xs = Right $ take pos xs ++ ys ++ drop (pos+len) xs
  | otherwise = Left $ "Segment " ++ show (pos,len) ++ " outside list of length " ++ show (length xs)


isSegmentOf :: Int -> Int -> [a] -> Either String Bool
isSegmentOf (-1) _ _ = Left "negative position"
isSegmentOf _ (-1) _ = Left "negative length"
isSegmentOf 1 0 [] = Right True -- empty segmnet just after empty list still ok
isSegmentOf pos len xs = Right $ pos + len <= length xs


-- List based operations

findSubstrings :: (Eq a) => [a] -> [a] -> [Int]
findSubstrings needle haystack = findIndices (isPrefixOf needle) $ tails haystack

replaceSubstrings :: (Eq a) => [a] -> [a] -> [a] -> [[a]]
replaceSubstrings xs ys zs = map replaceAt $ findSubstrings xs zs
  where
    replaceAt pos = take pos zs ++ ys ++ drop (pos + length xs) zs


-- TODO:
-- - redefine isSegmentOf to return strictly Bool (if not used elsewhere), replacing False for Left error?
-- - better replaceSubstrings?

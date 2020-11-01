{-# LANGUAGE TypeSynonymInstances #-}

module Miu
  ( Theorem
  , readTheorem
  , rule1
  , rule2
  , rule3
  , rule4
--  , deriveTheorems
  ) where

--import Control.Monad (join)
--import Data.Traversable (traverse)

import Lib (replaceSubstrings)


data Letter = M | I | U deriving (Eq, Show)
newtype Theorem = Theorem [Letter] deriving (Eq, Show)

readLetter :: Char -> Letter
readLetter c = case c of 'M' -> M
                         'I' -> I
                         'U' -> U
                         _ -> error "Invalid letter"

readLetters :: String -> [Letter]
readLetters = map readLetter

readTheorem :: String -> Theorem
readTheorem = Theorem . readLetters


-- MIU Rules

-- (I) MxI -> MxIU
rule1 :: Theorem -> [Theorem]
rule1 (Theorem t@(M:xs)) | last xs == I = [Theorem (t ++ [U])]
rule1 _ = []

-- (II) Mx -> Mxx
rule2 :: Theorem -> [Theorem]
rule2 (Theorem t@(M:xs)) = [Theorem (t ++ xs)]
rule2 _ = []

-- (III) MxIIIy -> MxUy
rule3 :: Theorem -> [Theorem]
rule3 (Theorem (M:ls)) = map (Theorem . (M:)) $ replaceSubstrings [I, I, I] [U] ls
rule3 _ = []

-- (IV) MxUUy -> Mxy
rule4 :: Theorem -> [Theorem]
rule4 (Theorem (M:ls)) = map (Theorem . (M:)) $ replaceSubstrings [U, U] [] ls
rule4 _ = []


-- deriveNextTheorems :: Theorem -> Either String [Theorem]
-- deriveNextTheorems t@(Theorem ls) = sequenceA $ filter isRight $ rules <*> pure t
--   where rules = concat [ [rule1]
--                        , [rule2]
--                        , [rule3 pos | pos <- [0..length ls]]
--                        , [rule4 pos | pos <- [0..length ls]]
--                        ]

-- deriveTheorems :: Either String [Theorem] -> Either String [[Theorem]]
-- --deriveTheorems ts = join $ (traverse deriveNextTheorems) <$> ts
-- deriveTheorems ts = traverse deriveNextTheorems =<< ts


-- TODO:
-- - make Theorem an instance of Data.ListLike ?
-- - should Theorem just be a newtype of Letters? Should it be more generic, `data Theorem a = Theorem a` or something?
-- - rule3 and rule4 are very ugly... and with redundant code

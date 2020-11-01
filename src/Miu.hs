{-# LANGUAGE TypeSynonymInstances #-}

module Miu
  ( Theorem
  , readTheorem
  , rule1
  , rule2
  , rule3
  , rule4
  , deriveTheorems
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
rule1 (Theorem [M]) = []
rule1 (Theorem t@(M:xs)) | last xs == I = [Theorem (t ++ [U])]
rule1 _ = []

-- (II) Mx -> Mxx
rule2 :: Theorem -> [Theorem]
rule2 (Theorem [M]) = []
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


deriveTheorems :: [Theorem] -> [[Theorem]]
deriveTheorems = iterate deriveNextTheorems
  where deriveNextTheorems ts = concat $ [rule1, rule2, rule3, rule4] <*> ts


-- TODO:
-- - make Theorem an instance of Data.ListLike ?
-- - should Theorem just be a newtype of Letters? Should it be more generic, `data Theorem a = Theorem a` or something?
-- - add a write monad to log what rules where applied at each step?
-- - definitely make it more efficient:
--    - remove identicaly theorems from derivation step
--    - cut loops, don't derive previously seen theorems

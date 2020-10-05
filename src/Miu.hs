{-# LANGUAGE TypeSynonymInstances #-}

module Miu
  ( Theorem
  , readTheorem
  , getLetters
  , readLetters
  , rule1
  , rule2
  , rule3
  , rule4
  ) where

--import Data.Either
import Lib
  ( getSegment
  , replaceSegment
  )


data Letter = M | I | U deriving (Eq, Show)
type Letters = [Letter]
data Theorem = Theorem Letters deriving (Eq, Show)
--newtype Theorem = Theorem {unTheorem letters :: Letters}

readLetter :: Char -> Letter
readLetter c = case c of 'M' -> M
                         'I' -> I
                         'U' -> U
                         _ -> error "Invalid letter"

readLetters :: String -> Letters
readLetters = map readLetter

readTheorem :: String -> Theorem
readTheorem = Theorem . readLetters

getLetters :: Theorem -> Letters
getLetters (Theorem xs) = xs


-- Rules

-- (I) MxI -> MxIU
rule1 :: Theorem -> Either String Theorem
rule1 (Theorem t@(M:xs)) | last xs == I = Right $ Theorem $ t ++ [U]
rule1 t = Left $ "Cannot apply (I) to " ++ (show t)

-- (II) Mx -> Mxx
rule2 :: Theorem -> Either String Theorem
rule2 (Theorem t@(M:xs)) = Right $ Theorem $ (t ++ xs)
rule2 t = Left $ "Cannot apply (II) to " ++ (show t)

-- (III) MxIIIy -> MxUy
rule3 :: Int -> Theorem -> Either String Theorem
rule3 pos (Theorem (M:xs))
  | getSegment (pos-1) 3 xs == (Right $ readLetters "III") = Theorem <$> (Right (M:) <*> ((replaceSegment (pos-1) 3 xs $ readLetters "U")))
rule3 pos t = Left $ "Cannot apply (III) to " ++ (show t) ++ " at " ++ show pos

-- -- (IV) MxUUy -> Mxy
rule4 :: Int -> Theorem -> Either String Theorem
rule4 pos (Theorem t@(M:xs))
  | getSegment (pos-1) 2 xs == (Right $readLetters "UU") = Theorem <$> (Right (M:) <*> replaceSegment (pos-1) 2 xs [])
rule4 pos t = Left $ "Cannot apply (IV) to " ++ (show t) ++ " at " ++ show pos


-- TODO:
-- - make Theorem an instance of Data.ListLike ?
-- - should Theorem just be a newtype of Letters? Should it be more generic, `data Theorem a = Theorem a` or something?
-- - rule3 and rule4 are very ugly... and with redundant code

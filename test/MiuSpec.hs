module MiuSpec where

import Data.Either       (isLeft, fromRight)
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Miu
  ( Theorem
  , readTheorem
  , rule1
  , rule2
  , rule3
  , rule4
  )

-- main :: IO ()
-- main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec = do
  describe "rule I" $ for_ rule1Cases (rule12Test rule1)
  describe "rule II" $ for_ rule2Cases (rule12Test rule2)
  describe "rule III" $ for_ rule3Cases (rule34Test rule3)
  describe "rule IV" $ for_ rule4Cases (rule34Test rule4)

  where
    rule12Test r (l, t, expected) = it description assertion
      where
        description = l ++ " " ++ show t
        assertion = (r $ readTheorem t) `shouldSatisfy` expected

    --rule1Cases :: [(String, Int, Int, [Int], (Either a Bool) -> Bool)]
    rule1Cases =
      [ ("does not start with M", "UI", isLeft)
      , ("does not end with I", "MU", isLeft)
      , ("does not end with I", "MIU", isLeft)
      , ("adds U after I", "MI", matchesRightTheorem "MIU")
      , ("adds U after I", "MII", matchesRightTheorem "MIIU")
      , ("adds U after I", "MIUI", matchesRightTheorem "MIUIU")
      ]

    rule2Cases =
      [ ("does not start with M", "UI", isLeft)
      , ("doubles string after M", "MI", matchesRightTheorem "MII")
      , ("doubles string after M", "MU", matchesRightTheorem "MUU")
      , ("doubles string after M", "MIU", matchesRightTheorem "MIUIU")
      , ("doubles string after M", "MUI", matchesRightTheorem "MUIUI")
      , ("doubles string after M", "MIIIUIU", matchesRightTheorem "MIIIUIUIIIUIU")
      ]

    rule34Test r (l, t, p, expected) = it description assertion
      where
        description = l ++ " " ++ show t
        assertion = (r p $ readTheorem t) `shouldSatisfy` expected

    rule3Cases =
      [ ("does not start with M", "UI", 0, isLeft)
      , ("does not start with M", "UIII", 1, isLeft)
      , ("cannot apply at position", "MI", 1, isLeft)
      , ("cannot apply at position", "MII", 1, isLeft)
      , ("cannot apply at position", "MIII", 2, isLeft)
      , ("cannot apply at position", "MIUII", 1, isLeft)
      , ("replaces III with U", "MIII", 1, matchesRightTheorem "MU")
      , ("replaces III with U", "MIUIII", 3, matchesRightTheorem "MIUU")
      , ("replaces III with U", "MIIIU", 1, matchesRightTheorem "MUU")
      , ("replaces III with U", "MIIII", 1, matchesRightTheorem "MUI")
      , ("replaces III with U", "MIIII", 2, matchesRightTheorem "MIU")
      ]

    rule4Cases =
      [ ("does not start with M", "UI", 0, isLeft)
      , ("does not start with M", "UUII", 1, isLeft)
      , ("cannot apply at position", "MU", 1, isLeft)
      , ("cannot apply at position", "MUU", 2, isLeft)
      , ("cannot apply at position", "MUIU", 1, isLeft)
      , ("cannot apply at position", "MUIU", 2, isLeft)
      , ("removes UU", "MUU", 1, matchesRightTheorem "M")
      , ("removes UU", "MIUU", 2, matchesRightTheorem "MI")
      , ("removes UU", "MIUUIUU", 2, matchesRightTheorem "MIIUU")
      , ("removes UU", "MIUUIUU", 5, matchesRightTheorem "MIUUI")
      ]


matchesRight :: (Eq b) => b -> Either a b -> Bool
matchesRight x = (fromRight False) . (fmap (== x))

matchesRightTheorem :: String -> Either a Theorem ->  Bool
matchesRightTheorem x = matchesRight (readTheorem x)

-- TODO:
-- - use text formatting for descriptions
-- - how to show expected values?
-- - format code

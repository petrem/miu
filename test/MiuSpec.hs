module MiuSpec where

import Data.Either       (isLeft)
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
  describe "rule I" $ for_ rule1Cases (ruleTest rule1)
  describe "rule II" $ for_ rule2Cases (ruleTest rule2)
  describe "rule III" $ for_ rule3Cases (ruleTest rule3)
  describe "rule IV" $ for_ rule4Cases (ruleTest rule4)

  where
    ruleTest r (l, t, expected) = it description assertion
      where
        description = l ++ " " ++ show t
        assertion = r (readTheorem t) `shouldBe` map readTheorem expected

    --rule1Cases :: [(String, Int, Int, [Int], (Either a Bool) -> Bool)]
    rule1Cases =
      [ ("does not start with M", "UI", [])
      , ("does not end with I", "MU", [])
      , ("does not end with I", "MIU", [])
      , ("adds U after I", "MI", ["MIU"])
      , ("adds U after I", "MII", ["MIIU"])
      , ("adds U after I", "MIUI", ["MIUIU"])
      ]

    rule2Cases =
      [ ("does not start with M", "UI", [])
      , ("doubles string after M", "MI", ["MII"])
      , ("doubles string after M", "MU", ["MUU"])
      , ("doubles string after M", "MIU", ["MIUIU"])
      , ("doubles string after M", "MUI", ["MUIUI"])
      , ("doubles string after M", "MIIIUIU", ["MIIIUIUIIIUIU"])
      ]

    rule3Cases =
      [ ("does not start with M", "UI", [])
      , ("does not start with M", "UIII", [])
      , ("cannot apply", "MI", [])
      , ("cannot apply", "MII", [])
      , ("cannot apply", "MIUII", [])
      , ("replaces III with U", "MIII", ["MU"])
      , ("replaces III with U", "MIUIII", ["MIUU"])
      , ("replaces III with U", "MIIIU", ["MUU"])
      , ("replaces III with U", "MIIII", ["MUI", "MIU"])
      , ("replaces III with U", "MIUIIIIUIII", ["MIUUIUIII", "MIUIUUIII", "MIUIIIIUU"])
      ]

    rule4Cases =
      [ ("does not start with M", "UI", [])
      , ("does not start with M", "UUII", [])
      , ("cannot apply", "MU", [])
      , ("cannot apply", "MUIU", [])
      , ("removes UU", "MUU", ["M"])
      , ("removes UU", "MIUU", ["MI"])
      , ("removes UU", "MIUUIUU", ["MIIUU", "MIUUI"])
      , ("removes UU", "MIUUUIUU", ["MIUIUU", "MIUIUU", "MIUUUI"])
      ]


matchesRight :: (Eq b) => b -> Either a b -> Bool
matchesRight x = either (const False) (== x)

matchesRightTheorem :: String -> Either a Theorem ->  Bool
matchesRightTheorem x = matchesRight (readTheorem x)

-- TODO:
-- - use text formatting for descriptions
-- - how to show expected values?
-- - format code

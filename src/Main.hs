-- TODO MUCH Faster rendering.
-- TODO GHCJS?
-- TODO Record more data.
-- TODO Lists instead of structs/bools?
--   This: [Calories, Teeth, OneMeal]
--   Instad of: emptyMeal {calories=True, teeth=True, oneMeal=True}
--   Yeah! Then just score with (length . nub).

{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# LANGUAGE OverloadedLists           #-}

module Main where

import Diagrams.Prelude hiding (stretch)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import Text.Printf
import Data.Monoid.Unicode
import Data.Set (Set)
import qualified Data.Set  as S
import qualified Data.Map  as M
import qualified Data.List as L

data Four a = Four a a a a
  deriving (Eq,Ord,Show,Functor)

data Entry = Good (Maybe Double)
           | Meh (Maybe Double)
           | Bad (Maybe Double)
           | Idk
           | NA
  deriving (Eq,Ord,Show)

data JulyDay = JulyDay {
    jlUptime    ∷ Entry
  , jlWaist     ∷ Entry
  , jlWeight    ∷ Entry
  , jlMeals     ∷ Four Entry
  , jlStretch   ∷ Entry
  , jlClean     ∷ Entry
  , jlWork      ∷ Entry
  , jlCantonese ∷ Entry
  , jlCreate    ∷ Entry
  , jlSocial    ∷ Entry
  , jlGym       ∷ Entry
  } deriving (Eq,Ord,Show)

type IntMap a = [(Int,a)]
type Month d = IntMap d

idk,success,failed ∷ Entry
(idk,success,failed) = (Idk, Good Nothing, Bad Nothing)

good,bad,meh ∷ Double → Entry
(good,bad,meh) = (Good . Just, Bad . Just, Meh . Just)

t,f ∷ Bool
(t,f) = (True, False)

n ∷ Maybe a
n = Nothing

j ∷ a → Maybe a
j = Just

fourSquares (Four a b c d) = ((a === c) ||| (b === d)) # center # scale 0.5

txtSquare t color = (text t # bold # scale(0.4)) ⊕ (square 1 # fc color)

symSquare t color = (text t # bold # scale(1/2)) ⊕ (square 1 # fc color)

showNum ∷ Double → String
showNum d = if noPoint then show (i `div` 10) else show d
  where i ∷ Int = round (d*10)
        noPoint = 0 == (i `mod` 10)

dispEntry ∷ Entry → Diagram B
dispEntry (Good (Just n)) = txtSquare (showNum n) forestgreen
dispEntry (Good Nothing)  = symSquare "✓"         forestgreen
dispEntry (Meh (Just n))  = txtSquare (showNum n) tomato
dispEntry (Meh Nothing)   = symSquare "·"         tomato
dispEntry (Bad (Just n))  = txtSquare (showNum n) firebrick
dispEntry (Bad Nothing)   = symSquare "✗"         firebrick
dispEntry NA              = txtSquare "⊥"         darkgrey
dispEntry Idk             = txtSquare "¿"         lightgrey

dispMeals = fourSquares . fmap (lwG(1/80) . dispEntry)

colLabel ∷ String → Diagram B
colLabel l = (text l # scale (2/3))
           ⊕ (square 1 # scaleY 1 # fc darkseagreen)

rowLabel ∷ String → Diagram B
rowLabel "" = square 1 # scaleX 4 # lwG 0
rowLabel l = (text l # scale (2/3)) ⊕ (square 1 # scaleX 4 # fc darkseagreen)

sheetLabel ∷ String → Diagram B
sheetLabel l = (text l # scale (2/3))
             ⊕ (square 1 # scaleY 1 # scaleX 4 # fc darkseagreen)

rowLabels ∷ String → Diagram B
rowLabels month = vcat (square 1 # lwG 0 : sheetLabel month # lwG (1/40) : rowNames)
  where rowNames = lwG (1/40) . rowLabel <$>
          [ "canto", "meals", "fit", "blocks", "org", "", "wakeTime", "getup"
          , "get-started", "weight", "waist", "squat", "bench", "dead", "chin"
          , "press", "purdy" ]

dispJulyDay ∷ Int → JulyDay → Diagram B
dispJulyDay dayNum day = case day of
  JulyDay{..} →
    vcat
      [ square 1               # lwG 0
      , colLabel (show dayNum) # lwG (1/40)
      , dispEntry jlUptime     # lwG (1/80)
      , dispEntry jlWaist      # lwG (1/80)
      , dispEntry jlWeight     # lwG (1/80)
      , dispEntry jlWork       # lwG (1/80)
      , dispMeals jlMeals
      , dispEntry jlStretch    # lwG (1/80)
      , dispEntry jlGym        # lwG (1/80)
      , dispEntry jlCantonese  # lwG (1/80)
      , dispEntry jlCreate     # lwG (1/80)
      , dispEntry jlSocial     # lwG (1/80)
      , dispEntry jlClean      # lwG (1/80)
      ]

pointEntry ∷ Maybe Int → Entry
pointEntry Nothing        = Idk
pointEntry (Just x) | x<1 = bad  $ fromIntegral x
pointEntry (Just x) | x<2 = bad  $ fromIntegral x
pointEntry (Just x) | x<3 = meh  $ fromIntegral x
pointEntry (Just x) | x<4 = meh  $ fromIntegral x
pointEntry (Just x)       = good $ fromIntegral x

-- dispPoint = dispEntry . pointEntry

dispPoint ∷ Maybe Int → Diagram B
dispPoint = dispEntry . pointEntry

dispPoint' ∷ Maybe Int → Diagram B
dispPoint' Nothing  = dispEntry Idk
dispPoint' (Just n) = (text (showNum $ fromIntegral n) # bold # scale(2/3)) ⊕ (square 1 # fc color)
  where color = case n of x | x<1 → firebrick
                          x | x<2 → firebrick
                          x | x<3 → tomato
                          x | x<4 → tomato
                          _       → forestgreen

dispStat = dispEntry . pointEntry

dAugMeals = fourSquares . fmap (lwG(1/80) . dispPoint')

dispAugustDay ∷ Int → AugustDay → Diagram B
dispAugustDay dayNum day = case day of
  AugustDay{..} → vcat [ square 1                           # lwG 0
                       , colLabel (show dayNum)             # lwG (1/40)
                       , dispPoint (score <$> auCanto)      # lwG (1/80)
                       , dAugMeals (fmap score <$> auMeals)
                       , dispPoint (score <$> auFit)        # lwG (1/80)
                       , dispPoint (score <$> auBlocks)     # lwG (1/80)
                       , dispPoint (score <$> auOrg)        # lwG (1/80)
                       , square 1                           # lwG 0
                       , dispEntry auWakeUp               # lwG (1/80)
                       , dispEntry auGetUp                  # lwG (1/80)
                       , dispEntry auGetStarted             # lwG (1/80)
                       , dispEntry auWeight                 # lwG (1/80)
                       , dispEntry auWaist                  # lwG (1/80)
                       , dispEntry auSquat                  # lwG (1/80)
                       , dispEntry auBench                  # lwG (1/80)
                       , dispEntry auDead                   # lwG (1/80)
                       , dispEntry auChin                   # lwG (1/80)
                       , dispEntry auPress                  # lwG (1/80)
                       , dispEntry auPurdy                  # lwG (1/80)
                       ]

dispJuly ∷ Month JulyDay → Diagram B
dispJuly m = rowLabels "July" ||| (hcat $ (\(n,d) → dispJulyDay n d) <$> m)

dispAugust ∷ Month AugustDay → Diagram B
dispAugust m = rowLabels "August" ||| (hcat $ (\(n,d) → dispAugustDay n d) <$> m)

main = mainWith $ dispAugust august


-- July ------------------------------------------------------------------------

julyBlank ∷ JulyDay
julyBlank = JulyDay Idk Idk Idk (Four idk idk idk idk) idk idk Idk Idk idk idk idk

july ∷ Month JulyDay
july =
  [ ( 1, JulyDay
        { jlUptime    = bad (7.5 - 3)
        , jlCantonese = failed
        , jlWaist     = good 38.2
        , jlWeight    = Idk
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlClean     = failed
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = failed
        })

  , ( 2, JulyDay
        { jlUptime    = good (5.5 - 3)
        , jlCantonese = failed
        , jlWaist     = good 38.2
        , jlWeight    = Idk
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = success
        })

  , ( 3, JulyDay
        { jlUptime    = bad (7.5 - 3)
        , jlCantonese = failed
        , jlWaist     = good 38.2
        , jlWeight    = Idk
        , jlMeals     = Four failed success success failed
        , jlStretch   = failed
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = failed
        })

  , ( 4, JulyDay
        { jlUptime    = bad (8 - 3)
        , jlClean     = failed
        , jlCantonese = failed
        , jlMeals     = Four failed failed failed failed
        , jlStretch   = success
        , jlGym       = failed
        , jlWeight    = Idk
        , jlWaist     = Idk
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 5, JulyDay
        { jlUptime    = bad (9 - 3)
        , jlClean     = failed
        , jlCantonese = failed
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlGym       = failed
        , jlWeight    = Idk
        , jlWaist     = good 38.3
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 6, JulyDay
        { jlUptime    = good (6 - 3)
        , jlClean     = failed
        , jlCantonese = good 4
        , jlMeals     = Four success failed failed failed
        , jlStretch   = failed
        , jlGym       = success
        , jlWeight    = Idk
        , jlWaist     = good 38.1
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 7, JulyDay
        { jlUptime    = bad (10.5 - 3)
        , jlClean     = success
        , jlCantonese = good 4
        , jlMeals     = Four idk idk idk idk
        , jlStretch   = failed
        , jlGym       = failed
        , jlWeight    = Idk
        , jlWaist     = good 37.9
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 8, JulyDay
        { jlUptime    = bad (10 - 3)
        , jlClean     = failed
        , jlCantonese = good 4
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlGym       = success
        , jlWeight    = Idk
        , jlWaist     = good 38.7
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 9, JulyDay
        { jlUptime    = bad (7 - 3)
        , jlClean     = failed
        , jlCantonese = good 4
        , jlMeals     = Four idk idk idk idk
        , jlStretch   = success
        , jlGym       = success
        , jlWeight    = Idk
        , jlWaist     = Idk
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 10, JulyDay
        { jlUptime    = bad (10 - 3)
        , jlClean     = idk
        , jlCantonese = good 4
        , jlMeals     = Four idk idk idk idk
        , jlStretch   = success
        , jlGym       = success
        , jlWeight    = Idk
        , jlWaist     = Idk
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        })

  , ( 11, JulyDay
        { jlUptime    = bad 11
        , jlClean     = idk
        , jlCantonese = failed
        , jlMeals     = Four idk idk idk idk
        , jlStretch   = failed
        , jlGym       = failed
        , jlWeight    = Idk
        , jlWaist     = Idk
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = idk
        })

  , ( 12, JulyDay
        { jlUptime    = good 6
        , jlCantonese = failed
        , jlWaist     = good 38.3
        , jlWeight    = bad 221
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = success
        , jlSocial    = failed
        , jlGym       = failed
        })

  , ( 13, JulyDay
        { jlUptime    = good 5.5
        , jlCantonese = good 2.5 -- From 6-8:30.
        , jlWaist     = good 37.6
        , jlWeight    = good 219
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 9
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = failed
        })
  , ( 14, JulyDay
        { jlUptime    = good 6
        , jlCantonese = good 2 -- From 6:30 - 8:30
        , jlWaist     = good 38.0
        , jlWeight    = good 218
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 7
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = success
        })
  , ( 15, JulyDay
        { jlUptime    = bad 6.5
        , jlCantonese = failed
        , jlWaist     = good 37.6
        , jlWeight    = good 216
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlClean     = success
        , jlWork      = good 8
        , jlCreate    = failed
        , jlSocial    = success -- Talked with peeps on fb.
        , jlGym       = failed
        })

  , ( 16, JulyDay
        { jlUptime    = good 5.8
        , jlCantonese = good 1.5 -- 6:30-8
        , jlWaist     = good 37.6
        , jlWeight    = good 216
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 7
        , jlCreate    = failed
        , jlSocial    = failed
        , jlGym       = failed
        })

  , ( 17, JulyDay
        { jlUptime    = good 5.8
        , jlCantonese = good 1 -- 6.5-7.5
        , jlWaist     = good 37.3
        , jlWeight    = good 216
        , jlMeals     = Four success success failed failed
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 7
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = failed
        })

  , ( 18, JulyDay
        { jlUptime    = good 5.8
        , jlCantonese = failed
        , jlWaist     = good 37.5
        , jlWeight    = good 218
        , jlMeals     = Four failed failed failed failed
        , jlStretch   = failed
        , jlClean     = failed
        , jlWork      = NA
        , jlCreate    = failed
        , jlSocial    = success
        , jlGym       = failed
        })

  , ( 19, JulyDay
        { jlUptime    = good 5.5
        , jlCantonese = failed
        , jlWaist     = good 37.6
        , jlWeight    = bad 220
        , jlMeals     = Four success failed failed failed
        , jlStretch   = failed
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = success
        , jlSocial    = success
        , jlGym       = failed
        })

  , ( 20, JulyDay
        { jlUptime    = good 5.5
        , jlCantonese = good 1
        , jlWaist     = good 37.6
        , jlWeight    = good 218
        , jlMeals     = Four success success success failed -- Too many calories.
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 12
        , jlCreate    = failed
        , jlSocial    = success -- Talked briefly with with Tim and Misha.
        , jlGym       = failed
        })

  , ( 21, JulyDay
        { jlUptime    = good 6 -- Woke up at 5:30.
        , jlCantonese = good 1 -- 20-21
        , jlWaist     = good 37.6
        , jlWeight    = good 217
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 8
        , jlCreate    = success
        , jlSocial    = success -- (20-21:30) Awkwardly hang with Misha and Jessica.
        , jlGym       = good 157 -- 5x135
        })

  , ( 22, JulyDay
        { jlUptime    = good 5.7 -- Woke up at 5:20.
        , jlCantonese = good 1.5
        , jlWaist     = good 37.5
        , jlWeight    = good 214
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 9
        , jlCreate    = success -- Shity recording of guitar piece: Exercise 72 from Play Classical Guitar
        , jlSocial    = success -- Dinner with Jessica, jlBreifly hung with Misha/Jessica.
        , jlGym       = NA
        })

  , ( 23, JulyDay
        { jlUptime    = good 6 -- Woke up at 5:20.
        , jlCantonese = good 3.3 -- 6:45-8:05, 9:45-10:45, 11:45-12:45
        , jlWaist     = good 37.2
        , jlWeight    = good 214
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = failed
        , jlWork      = good 9
        , jlCreate    = success -- Another guitar recording. It's on tumblr (private).
        , jlSocial    = success -- Hung out with Jessica on her lunch break.
        , jlGym       = failed
        })

  , ( 24, JulyDay
        { jlUptime    = bad 6.25
        , jlCantonese = failed
        , jlWaist     = good 37.0
        , jlWeight    = good 217
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = good 8
        , jlCreate    = success
        , jlSocial    = success
        , jlGym       = good 169 -- 5x145
        })

  , ( 25, JulyDay
        { jlUptime    = good 5
        , jlCantonese = good 1
        , jlWaist     = good 37.8
        , jlWeight    = good 217
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = success
        , jlSocial    = success -- Went to Twin Peaks/Zazie with Jessica/Misha.
        , jlGym       = NA
        })

  , ( 26, JulyDay
        { jlUptime    = bad 8.5
        , jlCantonese = good 1
        , jlWaist     = good 37.5
        , jlWeight    = good 214
        , jlMeals     = Four success success success success
        , jlStretch   = success
        , jlClean     = success
        , jlWork      = NA
        , jlCreate    = success -- Guitar recording on Tumblr.
        , jlSocial    = success -- Spent lots of time with Jessica.
        , jlGym       = good 180 -- 5x155
        })

  , ( 27, JulyDay
        { jlUptime    = good 5.5
        , jlCantonese = good 2
        , jlWaist     = good 36.9
        , jlWeight    = good 215
        , jlMeals     = Four success success success success
        , jlStretch   = failed
        , jlClean     = failed
        , jlWork      = good 9
        , jlCreate    = failed
        , jlSocial    = success -- Colton, Dawn, Jessica
        , jlGym       = NA
        })

  , ( 28, JulyDay
        { jlUptime    = good 5.5
        , jlWaist     = good 37.3
        , jlWeight    = good 215
        , jlMeals     = Four success success success success
        , jlWork      = good 9

        , jlStretch   = success
        , jlGym       = failed

        , jlClean     = success
        , jlCantonese = good 2
        , jlCreate    = failed
        , jlSocial    = success -- Lunch with Jessica.
        })

  , ( 29, JulyDay
        { jlUptime    = bad 6.5 -- Out the door at 7:07
        , jlWaist     = good 37.1
        , jlWeight    = good 217
        , jlMeals     = Four success success success success
        , jlClean     = success
        , jlWork      = good 9
        , jlStretch   = success
        , jlGym       = success
        , jlCantonese = good 1
        , jlCreate    = failed
        , jlSocial    = failed
        })

  , ( 30, JulyDay
        { jlUptime    = good 5.9
        , jlWaist     = good 37.2
        , jlWeight    = good 216
        , jlMeals     = Four success success success success
        , jlWork      = good 9

        , jlStretch   = success
        , jlGym       = failed

        , jlClean     = failed
        , jlCantonese = bad 0.5
        , jlCreate    = success
        , jlSocial    = success -- Hung with Scott @ MixRank
        })

  , ( 31, JulyDay
        { jlUptime    = bad 7.5
        , jlWaist     = good 36.7
        , jlWeight    = good 214
        , jlMeals     = Four success success success failed

        , jlWork      = meh 4

        , jlGym       = failed
        , jlSocial    = failed
        , jlStretch   = success

        , jlCantonese = failed
        , jlClean     = failed
        , jlCreate    = failed
        })
  ]


-- August ----------------------------------------------------------------------

score ∷ Set a → Int
score = fromIntegral . S.size

type Points a = Maybe (Set a)

data AugustDay = AugustDay {
    auCanto  ∷ Points CantoPoint
  , auMeals  ∷ Four (Points MealPoint)
  , auFit    ∷ Points FitPoint
  , auBlocks ∷ Points BlockPoint
  , auOrg    ∷ Points OrgPoint
  , auWakeUp, auGetUp, auGetStarted, auWeight, auWaist, auSquat, auBench
  , auDead, auChin, auPress, auPurdy ∷ Entry
  , stats ∷ Stats
  } deriving (Eq,Ord)

augustBlank ∷ AugustDay
augustBlank = AugustDay {
    auCanto    = n
  , auMeals    = Four n n n n
  , auFit      = n
  , auBlocks   = n
  , auOrg      = n
  , auWakeUp = idk
  , auGetUp    = idk
  , auGetStarted = idk
  , auWeight   = idk
  , auWaist    = idk
  , auSquat    = idk
  , auBench    = idk
  , auDead     = idk
  , auChin     = idk
  , auPress    = idk
  , auPurdy    = idk
  , stats      = []
  }

august ∷ Month AugustDay
august = zip [1..31] $ (++ repeat augustBlank) $
  [ AugustDay { auCanto    = j[Listen1,Listen2,Listen3,Anki,PracticeWJess]
              , auMeals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500])
                                  (j[Under750,Under500])
                                  (j[Under750,Under500,Teeth])
              , auFit      = j[Stretch,Cardio,Lift] -- Walking,chins
              , auBlocks   = j[Block1,Block2]       -- Pottery, Chart
              , auOrg      = j[Clean1,Clean2,Chore1,Chore2]
              , auWakeUp = bad 6.0
              , auGetUp    = good 6.3
              , auGetStarted = NA
              , auWeight   = meh 216
              , auWaist    = meh 37.0
              , auSquat    = NA
              , auBench    = NA
              , auDead     = NA
              , auChin     = meh 216
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  , AugustDay { auCanto    = j[Listen1,Listen2,Listen3,Anki,PracticeWJess]
              , auMeals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,Teeth,NoFudge])
                                  (j[Under750,SingleSitting,Teeth])
                                  (j[Under750,SingleSitting,Teeth])
              , auFit      = j[BikeCommute,Cardio,Stretch]
              , auBlocks   = j[]
              , auOrg      = j[Clean1,Clean2,Chore1,Chore2,InboxZero]
              , auWakeUp = meh 5.6
              , auGetUp    = good 5.8
              , auGetStarted = NA
              , auWeight   = meh 217
              , auWaist    = meh 37.1
              , auSquat    = NA
              , auBench    = NA
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  -- Monday, Aug 3
  , AugustDay { auCanto    = j[]
              , auMeals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,SingleSitting,Teeth,NoFudge])
              , auFit      = j[BikeCommute,Cardio,Gym,Lift,Stretch]
              , auBlocks   = j[Block1,Block2,Block3,Block4]
              , auOrg      = j[Chore1,Chore2]

              , auWakeUp = good 5.3
              , auGetUp    = bad 6.1
              , auGetStarted = bad 8.25
              , auWeight   = meh 218
              , auWaist    = meh 37.2
              , auSquat    = meh 192
              , auBench    = NA
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  -- Tuesday, Aug 4
  , AugustDay { auCanto    = j[PracticeWJess,Listen1]
              , auMeals    = Four (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,SingleSitting,Teeth,NoFudge])
              , auFit      = j[Cardio] -- Chin-ups, walking along the bay.
              , auBlocks   = j[Block1,Block2,Block3,Block4,Block5] -- Work, Backups.
              , auOrg      = j[Chore1,Chore2] -- Backups, canceled services.

              , auWakeUp = good 5.3 -- 5:20
              , auGetUp    = bad 5.75
              , auGetStarted = bad 6.75
              , auWeight   = meh 217
              , auWaist    = meh 37.3
              , auSquat    = NA
              , auBench    = NA
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  -- Wednesday, Aug 5
  , AugustDay { auCanto    = j[Listen1,Listen2,PracticeWJess,Anki]
              , auMeals    = Four
                  (j[Under750,Under500,SingleSitting,NoFudge])       -- Soylent
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Banana/corn/pb/yogurt
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth])
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth])
              , auFit      = j[Lift,Cardio,Stretch]
              , auBlocks   = j[Block1,Block2,Block3,Block4,Block5] -- So much work.
              , auOrg      = j[]

              , auWakeUp = meh 6.3 -- 6:20
              , auGetUp    = bad 6.9
              , auGetStarted = NA
              , auWeight   = meh 217
              , auWaist    = meh 37.3
              , auSquat    = NA
              , auBench    = NA
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = [ (WakeUp,meh 6.3),(GetUpTimer,bad 35),(Weight,good 217),(Waist,good 37.3)
                             , (Chin,NA),(Purdy,NA)
                             ]
              }

  -- Thursday, Aug 6
  , AugustDay { auCanto    = j[PracticeWJess,Anki]
              , auMeals    = Four
                  (j[Under750,Under500,SingleSitting,NoFudge])       -- Soylent
                  (j[SingleSitting,NoFudge])                         -- Lunch with Tim.
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Starbucks, Pretzel
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Soylent
              , auFit      = j[Gym,Stretch,Lift,Cardio]
              , auBlocks   = j[Block1,Block2,Block3,Block4,Block5] -- work*4 + wagon
              , auOrg      = j[InboxZero]
              , auWakeUp = good 5
              , auGetUp    = bad 5.6
              , auGetStarted = bad 6.5
              , auWeight   = meh 216
              , auWaist    = meh 37.1
              , auSquat    = meh 198.3
              , auBench    = meh 110
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  -- Friday, Aug 7
  , AugustDay { auCanto    = j[Anki,PracticeWJess,Listen1]
              , auMeals    = Four
                  (j[Under750,SingleSitting,NoFudge,Teeth])          -- 2*mochi+½*cookie = ~700
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Soylent
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Dinner w/ Jessica
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Combined with ^
              , auFit      = j[BikeCommute,Cardio,Stretch]
              , auBlocks   = j[Block1,Block2,Block3,Block4] -- Work*4
              , auOrg      = j[InboxZero,Chore1]

              , auWakeUp = bad 7.5
              , auGetUp    = bad 8.1
              , auGetStarted = bad 9.5
              , auWeight   = meh 214
              , auWaist    = meh 37.0
              , auSquat    = NA
              , auBench    = NA
              , auDead     = NA
              , auChin     = NA
              , auPress    = NA
              , auPurdy    = NA
              , stats      = []
              }

  -- Saturday, Aug 8
  , AugustDay { auCanto    = j[Listen1,Listen2,Listen3,PracticeWJess]
              , auMeals    = Four
                  (j[Under750,Under500,SingleSitting,      NoFudge])
                  (j[Under750,         SingleSitting,Teeth,NoFudge])
                  (j[Under750,Under500,SingleSitting,Teeth        ])
                  (j[Under750,Under500,SingleSitting,Teeth        ])
              , auFit      = j[Cardio]
              , auBlocks   = j[]
              , auOrg      = j[]

              , auWakeUp     = good 4.8
              , auGetUp      = good 5.5
              , auGetStarted = good 6
              , auWeight     = meh 214
              , auWaist      = meh 36.7
              , auSquat      = NA
              , auBench      = NA
              , auDead       = NA
              , auChin       = NA
              , auPress      = NA
              , auPurdy      = NA
              , stats        = []
              }

  -- Sunday, August 9
  , AugustDay { auCanto    = j[]
              , auMeals    = Four
                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                  (n)
                  (n)

              , auFit      = j[]
              , auBlocks   = j[]
              , auOrg      = j[]

              , auWakeUp     = good 5
              , auGetUp      = bad  8.3
              , auGetStarted = bad  9
              , auWeight     = meh 213
              , auWaist      = meh 37.0
              , auSquat      = NA
              , auBench      = NA
              , auDead       = NA
              , auChin       = NA
              , auPress      = NA
              , auPurdy      = NA
              , stats        = []
              }
  ]

data MealPoint  = Under500 | Under750 | SingleSitting | Teeth | NoFudge deriving (Eq,Ord)
data CantoPoint = Listen1 | Listen2 | Listen3 | Anki | PracticeWJess    deriving (Eq,Ord)
data FitPoint   = BikeCommute | Cardio | Lift | Gym | Stretch           deriving (Eq,Ord)
data OrgPoint   = Clean1 | Clean2 | Chore1 | Chore2 | InboxZero         deriving (Eq,Ord)
data BlockPoint = Block1 | Block2 | Block3 | Block4 | Block5            deriving (Eq,Ord)

type Stats = M.Map Stat Entry

data Stat = WakeUp | GetUpTimer | CalTrainTimer
          | Weight | Waist
          | Squat  | Bench | Dead | Chin | Press | Purdy
          deriving (Eq,Ord)

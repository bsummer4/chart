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

module AugustData(august) where

import August

type IntMap a = [(Int,a)]
type Month d = IntMap d

idk,_success,_failed ∷ Entry
(idk,_success,_failed) = (Idk, Good Nothing, Bad Nothing)

good,bad,meh ∷ Double → Entry
(good,bad,meh) = (Good . Just, Bad . Just, Meh . Just)

n ∷ Maybe a
n = Nothing

j ∷ a → Maybe a
j = Just

-- August ----------------------------------------------------------------------

augustBlank ∷ August.Day
augustBlank = August.Day {
    canto    = n
  , meals    = Four n n n n
  , fit      = n
  , blocks   = n
  , org      = n
  , wakeUp = idk
  , getUp    = idk
  , getStarted = idk
  , weight   = idk
  , waist    = idk
  , squat    = idk
  , bench    = idk
  , dead     = idk
  , chin     = idk
  , press    = idk
  , purdy    = idk
  , stats      = []
  }

august ∷ Month August.Day
august = zip [1..31] $ (++ repeat augustBlank) $
  [ August.Day { canto    = j[Listen1,Listen2,Listen3,Anki,PracticeWJess]
              , meals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500])
                                  (j[Under750,Under500])
                                  (j[Under750,Under500,Teeth])
              , fit      = j[Stretch,Cardio,Lift] -- Walking,chins
              , blocks   = j[Block1,Block2]       -- Pottery, Chart
              , org      = j[Clean1,Clean2,Chore1,Chore2]
              , wakeUp = bad 6.0
              , getUp    = good 6.3
              , getStarted = NA
              , weight   = meh 216
              , waist    = meh 37.0
              , squat    = NA
              , bench    = NA
              , dead     = NA
              , chin     = meh 216
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  , August.Day { canto    = j[Listen1,Listen2,Listen3,Anki,PracticeWJess]
              , meals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,Teeth,NoFudge])
                                  (j[Under750,SingleSitting,Teeth])
                                  (j[Under750,SingleSitting,Teeth])
              , fit      = j[BikeCommute,Cardio,Stretch]
              , blocks   = j[]
              , org      = j[Clean1,Clean2,Chore1,Chore2,InboxZero]
              , wakeUp = meh 5.6
              , getUp    = good 5.8
              , getStarted = NA
              , weight   = meh 217
              , waist    = meh 37.1
              , squat    = NA
              , bench    = NA
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  -- Monday, Aug 3
  , August.Day { canto    = j[]
              , meals    = Four (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                                  (j[Under750,SingleSitting,Teeth,NoFudge])
              , fit      = j[BikeCommute,Cardio,Gym,Lift,Stretch]
              , blocks   = j[Block1,Block2,Block3,Block4]
              , org      = j[Chore1,Chore2]

              , wakeUp = good 5.3
              , getUp    = bad 6.1
              , getStarted = bad 8.25
              , weight   = meh 218
              , waist    = meh 37.2
              , squat    = meh 192
              , bench    = NA
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  -- Tuesday, Aug 4
  , August.Day { canto    = j[PracticeWJess,Listen1]
              , meals    = Four (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,Under500,SingleSitting,NoFudge])
                                  (j[Under750,SingleSitting,Teeth,NoFudge])
              , fit      = j[Cardio] -- Chin-ups, walking along the bay.
              , blocks   = j[Block1,Block2,Block3,Block4,Block5] -- Work, Backups.
              , org      = j[Chore1,Chore2] -- Backups, canceled services.

              , wakeUp = good 5.3 -- 5:20
              , getUp    = bad 5.75
              , getStarted = bad 6.75
              , weight   = meh 217
              , waist    = meh 37.3
              , squat    = NA
              , bench    = NA
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  -- Wednesday, Aug 5
  , August.Day { canto    = j[Listen1,Listen2,PracticeWJess,Anki]
              , meals    = Four
                  (j[Under750,Under500,SingleSitting,NoFudge])       -- Soylent
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Banana/corn/pb/yogurt
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth])
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth])
              , fit      = j[Lift,Cardio,Stretch]
              , blocks   = j[Block1,Block2,Block3,Block4,Block5] -- So much work.
              , org      = j[]

              , wakeUp = meh 6.3 -- 6:20
              , getUp    = bad 6.9
              , getStarted = NA
              , weight   = meh 217
              , waist    = meh 37.3
              , squat    = NA
              , bench    = NA
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = [ (WakeUp,meh 6.3),(GetUp,bad 35),(Weight,good 217),(Waist,good 37.3)
                             , (Chin,NA),(Purdy,NA)
                             ]
              }

  -- Thursday, Aug 6
  , August.Day { canto    = j[PracticeWJess,Anki]
              , meals    = Four
                  (j[Under750,Under500,SingleSitting,NoFudge])       -- Soylent
                  (j[SingleSitting,NoFudge])                         -- Lunch with Tim.
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Starbucks, Pretzel
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Soylent
              , fit      = j[Gym,Stretch,Lift,Cardio]
              , blocks   = j[Block1,Block2,Block3,Block4,Block5] -- work*4 + wagon
              , org      = j[InboxZero]
              , wakeUp = good 5
              , getUp    = bad 5.6
              , getStarted = bad 6.5
              , weight   = meh 216
              , waist    = meh 37.1
              , squat    = meh 198.3
              , bench    = meh 110
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  -- Friday, Aug 7
  , August.Day { canto    = j[Anki,PracticeWJess,Listen1]
              , meals    = Four
                  (j[Under750,SingleSitting,NoFudge,Teeth])          -- 2*mochi+½*cookie = ~700
                  (j[Under750,Under500,SingleSitting,NoFudge,Teeth]) -- Soylent
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Dinner w/ Jessica
                  (j[Under750,Under500,SingleSitting,Teeth])         -- Combined with ^
              , fit      = j[BikeCommute,Cardio,Stretch]
              , blocks   = j[Block1,Block2,Block3,Block4] -- Work*4
              , org      = j[InboxZero,Chore1]

              , wakeUp = bad 7.5
              , getUp    = bad 8.1
              , getStarted = bad 9.5
              , weight   = meh 214
              , waist    = meh 37.0
              , squat    = NA
              , bench    = NA
              , dead     = NA
              , chin     = NA
              , press    = NA
              , purdy    = NA
              , stats      = []
              }

  -- Saturday, Aug 8
  , August.Day { canto    = j[Listen1,Listen2,Listen3,PracticeWJess]
              , meals    = Four
                  (j[Under750,Under500,SingleSitting,      NoFudge])
                  (j[Under750,         SingleSitting,Teeth,NoFudge])
                  (j[Under750,Under500,SingleSitting,Teeth        ])
                  (j[Under750,Under500,SingleSitting,Teeth        ])
              , fit      = j[Cardio]
              , blocks   = j[]
              , org      = j[]

              , wakeUp     = good 4.8
              , getUp      = good 5.5
              , getStarted = good 6
              , weight     = meh 214
              , waist      = meh 36.7
              , squat      = NA
              , bench      = NA
              , dead       = NA
              , chin       = NA
              , press      = NA
              , purdy      = NA
              , stats        = []
              }

  -- Sunday, August 9
  , August.Day { canto    = j[]
              , meals    = Four
                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                  (j[Under750,Under500,SingleSitting,Teeth,NoFudge])
                  (n)
                  (n)

              , fit      = j[]
              , blocks   = j[]
              , org      = j[]

              , wakeUp     = good 5
              , getUp      = bad  8.3
              , getStarted = bad  9
              , weight     = meh 213
              , waist      = meh 37.0
              , squat      = NA
              , bench      = NA
              , dead       = NA
              , chin       = NA
              , press      = NA
              , purdy      = NA
              , stats        = [ (WakeUp,     good 5)
                               , (GetUp,      bad 8.3)
                               , (GetStarted, bad 9)
                               , (Weight,     meh 213)
                               , (Waist,      meh 37.0)
                               ]
              }
  ]

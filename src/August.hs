{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}

module August where

import qualified Data.Map        as M
import           Data.Set        (Set)
import           Data.Text       (Text)
import           Data.Time.Clock


-- Simple Types ----------------------------------------------------------------

type Points a = Maybe (Set a)

data Four a = Four a a a a
  deriving (Eq,Ord,Show,Functor)

data Entry = Good (Maybe Double)
           | Meh (Maybe Double)
           | Bad (Maybe Double)
           | Idk
           | NA
  deriving (Eq,Ord,Show)


-- Points ----------------------------------------------------------------------

data MealPoint  = Under500 | Under750 | SingleSitting | Teeth | NoFudge deriving (Eq,Ord)
data CantoPoint = Listen1 | Listen2 | Listen3 | Anki | PracticeWJess    deriving (Eq,Ord)
data FitPoint   = BikeCommute | Cardio | Lift | Gym | Stretch           deriving (Eq,Ord)
data OrgPoint   = Clean1 | Clean2 | Chore1 | Chore2 | InboxZero         deriving (Eq,Ord)
data BlockPoint = Block1 | Block2 | Block3 | Block4 | Block5            deriving (Eq,Ord)

data Stat = WakeUp | GetUp | GetStarted
          | Weight | Waist
          | Squat  | Bench | Dead | Chin | Press | Purdy
          deriving (Eq,Ord)


-- Chart Data per Day ----------------------------------------------------------

data Day = Day {
    canto  ∷ Points CantoPoint
  , meals  ∷ Four (Points MealPoint)
  , fit    ∷ Points FitPoint
  , blocks ∷ Points BlockPoint
  , org    ∷ Points OrgPoint

  , wakeUp, getUp, getStarted, weight, waist, squat, bench
  , dead, chin, press, purdy ∷ Entry

  , stats ∷ Stats
  } deriving (Eq,Ord)

type Stats = M.Map Stat Entry


-- Tags ------------------------------------------------------------------------

data TextTag = TextTag { tag∷Text, params∷Set Text }

data ValidTag = Sleep
              | Meal  (Set MealPoint)
              | Canto CantoPoint
              | Fit   FitPoint
              | Org   OrgPoint
              | Block BlockPoint

data Tag = InvalidTag TextTag
         | ValidTag   ValidTag


-- Processed Data about Events -------------------------------------------------

data EventData = EventData
  { start    ∷ UTCTime
  , duration ∷ NominalDiffTime
  , tags     ∷ [Tag]
  , name     ∷ Text
  , note     ∷ Text
  }

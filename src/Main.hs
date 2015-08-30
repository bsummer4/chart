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

module Main(main) where

import           August
import           AugustData                   (august)
import           Data.Monoid.Unicode
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Typeable
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (stretch)
import           Diagrams.TwoD.Text

type IntMap a = [(Int,a)]
type Month d = IntMap d

good,bad,meh ∷ Double → Entry
(good,bad,meh) = (Good . Just, Bad . Just, Meh . Just)

fourSquares ∷ (Eq (N b), Fractional (N b), Traversable (V b), Semigroup b,
               Additive (V b), Transformable b, Juxtaposable b, HasOrigin b,
               Alignable b, V b ~ V2)
            ⇒ Four b → b
fourSquares (Four a b c d) = ((a === c) ||| (b === d)) # center # scale 0.5

txtSquare ∷ (RealFloat n, Typeable n, Renderable (Path V2 n) b,
             Renderable (Text n) b)
          ⇒ String → Colour Double → QDiagram b V2 n Any
txtSquare t color = (text t # bold # scale(0.4)) ⊕ (square 1 # fc color)

symSquare ∷ (RealFloat n, Typeable n,
             Renderable (Path V2 n) b,
             Renderable (Diagrams.TwoD.Text.Text n) b)
          ⇒ String → Colour Double → QDiagram b V2 n Any
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
dispPoint' (Just i) = (text (showNum $ fromIntegral i) # bold # scale(2/3)) ⊕ (square 1 # fc color)
  where color = case i of x | x<1 → firebrick
                          x | x<2 → firebrick
                          x | x<3 → tomato
                          x | x<4 → tomato
                          _       → forestgreen

dAugMeals ∷ Four (Maybe Int) → QDiagram B V2 Double Any
dAugMeals = fourSquares . fmap (lwG(1/80) . dispPoint')

dispAugustDay ∷ Int → August.Day → Diagram B
dispAugustDay dayNum day = case day of
  August.Day{..} → vcat [ square 1                           # lwG 0
                       , colLabel (show dayNum)             # lwG (1/40)
                       , dispPoint (score <$> canto)      # lwG (1/80)
                       , dAugMeals (fmap score <$> meals)
                       , dispPoint (score <$> fit)        # lwG (1/80)
                       , dispPoint (score <$> blocks)     # lwG (1/80)
                       , dispPoint (score <$> org)        # lwG (1/80)
                       , square 1                           # lwG 0
                       , dispEntry wakeUp                 # lwG (1/80)
                       , dispEntry getUp                  # lwG (1/80)
                       , dispEntry getStarted             # lwG (1/80)
                       , dispEntry weight                 # lwG (1/80)
                       , dispEntry waist                  # lwG (1/80)
                       , dispEntry squat                  # lwG (1/80)
                       , dispEntry bench                  # lwG (1/80)
                       , dispEntry dead                   # lwG (1/80)
                       , dispEntry chin                   # lwG (1/80)
                       , dispEntry press                  # lwG (1/80)
                       , dispEntry purdy                  # lwG (1/80)
                       ]

dispAugust ∷ Month August.Day → Diagram B
dispAugust m = rowLabels "August" ||| (hcat $ (\(i,d) → dispAugustDay i d) <$> m)

main ∷ IO ()
main = mainWith $ dispAugust august

-- August ----------------------------------------------------------------------

score ∷ Set a → Int
score = fromIntegral . S.size

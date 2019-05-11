{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Text.Blaze.Svg11 ((!), mkPath, l, m, aa)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import qualified Recaman as R

main :: IO ()
main = do
  let svg = renderSvg svgDoc
  writeFile "./recaman.svg" svg

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "1000" ! A.height "1000" $ do
  svgRect (0,0) (500, 500)
  forM_ (zip [0..] (zip points (tail points))) (\(s, ps) -> svgArc (fst ps) (snd ps) ((==) (mod s 2) 0))
  where
    points = map (R.translate (250, 1) . R.scale (5, 5)) (R.recamanAsPoints 33)

svgRect :: R.Point -> R.Point -> S.Svg
svgRect (x,y) (w,h) = S.rect !
  A.x (S.stringValue (show x)) ! A.y (S.stringValue (show y)) !
  A.width (S.stringValue (show w)) ! A.height (S.stringValue (show h)) !
  A.fill "gray"

svgCircle :: R.Point -> Float -> S.Svg
svgCircle (x,y) r = S.circle !
  A.cx (S.stringValue (show x)) !
  A.cy (S.stringValue (show y)) !
  A.r (S.stringValue (show r)) !
  A.stroke "black" ! A.strokeWidth "3.0" ! A.fill "transparent"

svgArc :: R.Point -> R.Point -> Bool -> S.Svg
svgArc (x1,y1) (x2,y2) s = S.path !
  A.stroke "white" ! A.fill "transparent" ! A.strokeWidth "2.0" !
  A.d (mkPath (m x1 y1 >> aa 1 1 0 False side x2 y2))
  where
    p01 = sqrt ((x1 *x1) + (y1 * y1))
    p02 = sqrt ((x2 *x2) + (y2 * y2))
    side = if p02 > p01 then s else not s
module GlossyRuler where

import Graphics.Gloss (
  Color,
  Picture,
  color,
  pictures,
  rectangleSolid,
  translate,
 )

drawRuler :: (Float, Float) -> (Float, Float) -> Int -> Float -> Color -> Color -> Color -> Picture
drawRuler position dimensions numTickMarks measurement baseColor tickColor measurementColor =
  pictures $ map (translate posx posy) ([base] ++ tickMarks ++ [measurementMaker])
  where
    posx = fst position
    posy = snd position
    dimx = fst dimensions
    dimy = snd dimensions
    base = color baseColor $ rectangleSolid dimx dimy
    tickMarkDimensions = (10, 10)
    tickMarkDimx = fst tickMarkDimensions
    tickMarkDimy = snd tickMarkDimensions
    tickMarkOffset = -(tickMarkDimx / 4)
    tick = rectangleSolid tickMarkDimx tickMarkDimy
    tickStep = dimy / fromIntegral numTickMarks
    tickSteps = take numTickMarks $ iterate (* tickStep) 1
    tickMarks = map (\ts -> translate (-(dimx / 2)) ts $ color tickColor tick) tickSteps
    measurementMakerDimensions = (50, 10)
    measurementMarkerOffset = -(measurementMakerDimx / 4)
    measurementMakerDimx = fst measurementMakerDimensions
    measurementMakerDimy = snd measurementMakerDimensions
    measurementMaker = translate (-(measurementMakerDimx / 2) + measurementMarkerOffset) (measurement - dimy / 2) $ color measurementColor $ rectangleSolid measurementMakerDimx measurementMakerDimy

module GlossyRuler where
import Graphics.Gloss

drawRuler :: (Float, Float) -> (Float, Float) -> Float -> Float -> Color -> Color -> Color -> Picture
drawRuler position dimensions numTickMarks measurement baseColor tickColor measurementColor =
     pictures $ map (translate posx posy) [base, measurementMaker] ++ tickMarks
    where
        posx = fst position
        posy = snd position
        dimx = fst dimensions
        dimy = snd dimensions
        base = color baseColor $ rectangleSolid dimx dimy
        tickMarkDimensions = (50, 10)
        tickMarkDimx = fst tickMarkDimensions
        tickMarkDimy = snd tickMarkDimensions
        tickMarkOffset = - (tickMarkDimx / 4)
        tick = rectangleSolid tickMarkDimx tickMarkDimy
        tickStep = dimy / numTickMarks
        tickSteps = [tickStep .. tickStep * numTickMarks]
        tickMarks = map (\tickStep -> translate (- (tickMarkDimx / 2)) tickStep $ color measurementColor tick) tickSteps
        measurementMaker = translate (- (tickMarkDimx / 2) + tickMarkOffset) measurement $ color measurementColor tick
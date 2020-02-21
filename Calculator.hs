-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Expr
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoom    <- mkSlider (1,100) 50           -- Zoom slider, task J
     differ  <- mkButton "Differentiate"      -- Differentiate button, task K
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zoom, pure differ]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
     -- Draws an x-y grid
     path "black" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
     path "black" [(0,canHeight/2),(canWidth,canHeight/2)] canvas

     -- Interaction (install event handlers)
    --  on UI.click     differ $ \ _ -> readAndDraw <$> (diffInput input) <*> (pure zoom) <*> (pure canvas)
     on UI.click      differ $ \ _ ->   do
                                         formula <- get value input
                                         return input # set value (showExpr (differentiate $ fromJust $ readExpr formula))
                                         readAndDraw input zoom canvas

     on UI.click     draw  $ \ _ -> readAndDraw input zoom canvas
     on valueChange' input $ \ _ -> readAndDraw input zoom canvas
     on valueChange' zoom $ \ _ -> readAndDraw input zoom canvas


diffInput input = do 
                formula <- get value input
                let diffFormula = differentiate $ fromJust $ readExpr formula
                (return input) # set value (showExpr diffFormula)
-- I ---------------------------------------------------
readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input slider canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     zoom <- get value slider
     -- Clear the canvas
     let maybeExpr = readExpr formula
     let expr = fromJust maybeExpr
     clearCanvas canvas
     -- Have to redraw the grid each time
     path "black" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
     path "black" [(0,canHeight/2),(canWidth,canHeight/2)] canvas
     let zoomValue = (read zoom)/100
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
    -- path "blue" [(10,10),(canWidth-10,canHeight/2)] canvas
     path "blue" (points expr (0.02/zoomValue) (canWidth,canHeight)) canvas

-- H ------------------------------------------
points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (width,height) =  zip (map mathToUix xs) (map mathToUiy ys)

               where xs = map uiToMathx [0..width']
                     ys = map (eval expr) xs
                     height' = fromIntegral height
                     width' = fromIntegral width 
                     mathToUix x = (width' / 2) + (x/scale)
                     mathToUiy y = (height' / 2) - (y/scale)
                     uiToMathx x = -(width' * scale)/2 + (x*scale) -- Scale is needed for the graph to look smooth, becomes pointy otherwise


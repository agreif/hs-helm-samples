import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

data State = State {mx :: Double, my :: Double}

step :: (Int, Int) -> State -> State
step (dx, _) state = state {mx = mx state + realToFrac dx}

render :: State -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h [move (mx state, my state) $ filled red $ square 64]

main :: IO ()
main = do
  run config $ render <~ stepper ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "space_invaders",
                                windowDimensions = (w, h)}
        (w, h) = (500, 500)
        state = State {mx = 0, my = realToFrac h / 2 * 0.8}
        stepper = foldp step state Keyboard.arrows

import FRP.Helm
import qualified FRP.Helm.Window as Window

data State = State {mx :: Double, my :: Double}

step :: [Int] -> State -> State
step [dx, dy] state = state {mx = mx state + realToFrac dx,
                             my = my state + realToFrac dy}

render :: State -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h [move (mx state, my state) $ filled red $ square 64]

main :: IO ()
main = do
  run config $ render <~ stepper ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "random_sensor",
                                windowDimensions = (500, 500)}
        state = State {mx = 0, my = 0}
        stepper = foldp step state $ combine [randomR (-1, 1), randomR (-1, 1)]

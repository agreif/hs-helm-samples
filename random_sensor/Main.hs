import FRP.Helm
import qualified FRP.Helm.Window as Window

data State = State {mx :: Double, my :: Double, speed :: Double}

step :: [Int] -> State -> State
step [dx, dy] state = state {mx = mx state + realToFrac dx * speed state,
                             my = my state + realToFrac dy * speed state}

render :: State -> State -> State -> (Int, Int) -> Element
render state1 state2 state3 (w, h) = centeredCollage w h [move (mx state1, my state1) $ filled red $ square 64,
                                                          move (mx state2, my state2) $ filled blue $ square 64,
                                                          move (mx state2, my state3) $ filled yellow $ square 64]

main :: IO ()
main = do
  run config $ render <~ (stepper state1) ~~ (stepper state2) ~~ (stepper state3) ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "random_sensor",
                                windowDimensions = (500, 500)}
        state1 = State {mx = 0, my = -30, speed = 1}
        state2 = State {mx = 0, my = 30, speed = 2}
        state3 = State {mx = 0, my = 30, speed = 5}
        stepper state = foldp step state $ combine [randomR (-1, 1), randomR (-1, 1)]


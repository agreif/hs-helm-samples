import FRP.Helm
import qualified FRP.Helm.Window as Window

data State = State {mx :: Double, my :: Double, speed :: Double, color :: Color}

step :: [Int] -> State -> State
step [dx, dy] state = state {mx = mx state + realToFrac dx * speed state,
                             my = my state + realToFrac dy * speed state}

render :: [State] -> (Int, Int) -> Element
render states (w, h) = centeredCollage w h forms
  where forms = map (\state -> createForm state) states
        createForm :: State -> Form
        createForm state = move (mx state, my state) $ filled (color state) $ square 64

main :: IO ()
main = do
  run config $ render <~ squares ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "random_sensor",
                                windowDimensions = (500, 500)}
        state1 = State {mx = 0, my = -30, speed = 1, color = red}
        state2 = State {mx = 0, my = 30, speed = 2, color = blue}
        state3 = State {mx = 0, my = 60, speed = 5, color = yellow}
        stepper state = foldp step state $ combine [randomR (-1, 1), randomR (-1, 1)]
        squares = combine [(stepper state1), (stepper state2), (stepper state3)]

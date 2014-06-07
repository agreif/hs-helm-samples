import FRP.Helm
import qualified FRP.Helm.Window as Window

data State = State {mx :: Double, my :: Double, delay :: Int, color :: Color}

step :: [Int] -> State -> State
step [c, dx, dy] state = state {mx = mx state + realToFrac dx',
                                my = my state + realToFrac dy'}
  where dx' = if c `mod` (delay state) == 0 then dx else 0
        dy' = if c `mod` (delay state) == 0 then dy else 0

render :: [State] -> (Int, Int) -> Element
render states (w, h) = centeredCollage w h forms
  where forms = map (\state -> createForm state) states
        createForm :: State -> Form
        createForm state = move (mx state, my state) $ filled (color state) $ square 64

main :: IO ()
main = do
  run config $ render <~ squares ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "different_step_frequency",
                                windowDimensions = (500, 500)}
        state1 = State {mx = 0, my = -30, delay = 1, color = red}
        state2 = State {mx = 0, my = 30, delay = 10, color = blue}
        state3 = State {mx = 0, my = 60, delay = 100, color = yellow}
        stepper state = foldp step state $ combine [count, randomR (-1, 1), randomR (-1, 1)]
        squares = combine [(stepper state1), (stepper state2), (stepper state3)]

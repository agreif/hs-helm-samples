import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

data State = State {mx :: Double, my :: Double}

step1 :: (Int, Int) -> State -> State
step1 (dx, dy) state = state {mx = mx state + realToFrac dx,
                              my = my state + realToFrac dy}

step2 :: [Keyboard.Key] -> State -> State
step2 keys state = state {mx = mx state + dx, my = my state + dy}
  where dx1 = if Keyboard.AKey `elem` keys then -1 else 0
        dx2 = if Keyboard.DKey `elem` keys then 1 else 0
        dy1 = if Keyboard.WKey `elem` keys then -1 else 0
        dy2 = if Keyboard.SKey `elem` keys then 1 else 0
        dx = dx1 + dx2
        dy = dy1 + dy2

render :: State -> State -> (Int, Int) -> Element
render state1 state2 (w, h) =
  centeredCollage w h [move (mx state1, my state1) $ filled red $ square 64,
                       move (mx state2, my state2) $ filled blue $ square 64]

main :: IO ()
main = run config $ render <~ stepper1 ~~ stepper2 ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "keyboard_sensors",
                                windowDimensions = (500, 500)}
        state1 = State {mx = 0, my = 0}
        state2 = State {mx = 0, my = 100}
        stepper1 = foldp step1 state1 Keyboard.arrows
        stepper2 = foldp step2 state2 Keyboard.keysDown


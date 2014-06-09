import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Text as Text
import FRP.Elerea.Simple

data State = State {mx :: Double, my :: Double, color :: Color, text :: String}


stepper1 :: SignalGen(Signal State)
stepper1 = foldp step state1 Keyboard.arrows
  where state1 = State {mx = 0, my = 0, color = red, text = "A"}
        step :: (Int, Int) -> State -> State
        step (dx, dy) state = state {mx = mx state + realToFrac dx,
                                      my = my state + realToFrac dy}



stepper2 :: SignalGen(Signal State)
stepper2 = foldp step state2 signal
  where state2 = State {mx = 0, my = 100, color = blue, text = "B"}
        signal = lift2 combine' Keyboard.wasd stepper1
        combine' :: (Int, Int) -> State -> ((Int, Int), State)
        combine' (dx, dy) state1 = ((dx, dy), state1)
        step :: ((Int, Int), State) -> State -> State
        step ((dx, dy), state1) state2 = state2 {mx = mx state2 + realToFrac dx,
                                                 my = my state2 + realToFrac dy,
                                                 text = show distance}
          where distance = round $ sqrt ((mx state1 - mx state2)^2 + (my state1 - my state2)^2)


form :: State -> [Form]
form state = [move (mx state, my state) $ filled (color state) $ square 64,
              move (mx state, my state) $ toForm $ Text.plainText $ text state]

render :: State -> State -> (Int, Int) -> Element
render state1 state2 (w, h) =
  centeredCollage w h $ form state1 ++ form state2

main :: IO ()
main = run config $ render <~ stepper1 ~~ stepper2 ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "dependent_sensors",
                                windowDimensions = (500, 500)}


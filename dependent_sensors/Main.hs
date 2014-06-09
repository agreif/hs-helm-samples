import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Text as Text
import FRP.Elerea.Simple

data SquareState = SquareState {mx :: Double, my :: Double, color :: Color, text :: String}

redSquare :: SignalGen(Signal SquareState)
redSquare = foldp step initialState Keyboard.arrows
  where initialState = SquareState {mx = 0, my = 0, color = red, text = "A"}
        step :: (Int, Int) -> SquareState -> SquareState
        step (dx, dy) state = state {mx = mx state + realToFrac dx,
                                      my = my state + realToFrac dy}

blueSquare :: SignalGen(Signal SquareState)
blueSquare = foldp step initialState signal
  where initialState = SquareState {mx = 0, my = 100, color = blue, text = "B"}
        signal = lift2 (\(dx, dy) stateRed -> ((dx, dy), stateRed)) Keyboard.wasd redSquare
        step :: ((Int, Int), SquareState) -> SquareState -> SquareState
        step ((dx, dy), stateRed) stateBlue = stateBlue {mx = mx stateBlue + realToFrac dx,
                                                       my = my stateBlue + realToFrac dy,
                                                       text = show distance}
          where distance = round $ sqrt ((mx stateRed - mx stateBlue)^2
                                         + (my stateRed - my stateBlue)^2)

form :: SquareState -> [Form]
form state = [move (mx state, my state) $ filled (color state) $ square 64,
              move (mx state, my state) $ toForm $ Text.plainText $ text state]

render :: SquareState -> SquareState -> (Int, Int) -> Element
render stateRed stateBlue (w, h) =
  centeredCollage w h $ form stateRed ++ form stateBlue

main :: IO ()
main = run config $ render <~ redSquare ~~ blueSquare ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "dependent_sensors",
                                windowDimensions = (500, 500)}


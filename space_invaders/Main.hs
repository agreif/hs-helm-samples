import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import FRP.Elerea.Simple

data SpaceshipState = SpaceshipState {mx :: Double, my :: Double}

createSpaceship :: Int -> SignalGen(Signal SpaceshipState)
createSpaceship winHeight = spaceshipSignal
  where state = SpaceshipState {mx = 0, my = realToFrac winHeight / 2 * 0.8}
        spaceshipSignal = foldp moveSpaceship state Keyboard.arrows

moveSpaceship :: (Int, Int) -> SpaceshipState -> SpaceshipState
moveSpaceship (dx, _) state = state {mx = mx state + realToFrac dx}

render :: SpaceshipState -> (Int, Int) -> Element
render state (w, h) = centeredCollage w h [move (mx state, my state) $ filled red $ square 64]

main :: IO ()
main = do
  run config $ render <~ (createSpaceship winHeight) ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "space_invaders",
                                windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

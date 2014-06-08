import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import FRP.Elerea.Simple

data CannonState = CannonState {cx :: Double, cy :: Double}
data LaserState = LaserState {ly :: Double, flying :: Bool}

cannonSignal :: Int -> SignalGen(Signal CannonState)
cannonSignal winHeight = signal
  where initialState = CannonState {cx = 0, cy = realToFrac winHeight / 2 * 0.8}
        signal = foldp moveCannon initialState Keyboard.arrows
        moveCannon :: (Int, Int) -> CannonState -> CannonState
        moveCannon (dx, _) state = state {cx = cx state + realToFrac dx}

cannonForm :: CannonState -> Form
cannonForm state = move (cx state, cy state) $ filled red $ square 64

laserSignal :: SignalGen(Signal LaserState)
laserSignal = signal
  where initialState = LaserState {ly = 0, flying = False}
        signal = foldp fireLaser initialState Keyboard.space
        fireLaser :: Bool -> LaserState -> LaserState
        fireLaser fired state = state {flying = flying, ly = ly'}
          where flying = (fired && (ly state == 0)) || (ly state /= 0 && ly state < 300)
                ly' = if flying then ly state + 1 else 0

laserForm :: LaserState -> CannonState -> Form
laserForm laserState cannonState = move (lx', ly') $ filled white $ rect 2 10
  where lx' = cx cannonState
        ly' = cy cannonState - 30 - ly laserState

render :: CannonState -> LaserState -> (Int, Int) -> Element
render cannonState laserState (w, h) = centeredCollage w h [cannonForm cannonState,
                                                            laserForm laserState cannonState]

main :: IO ()
main = do
  run config $ render <~ (cannonSignal winHeight) ~~ laserSignal ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "space_invaders",
                                windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

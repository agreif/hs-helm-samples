import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import FRP.Elerea.Simple

data CannonState = CannonState {cx :: Double, cy :: Double,
                                lx :: Double, ly :: Double, laserFlying :: Bool}

cannonSignal :: Int -> SignalGen(Signal CannonState)
cannonSignal winHeight = signal
  where initialState = CannonState {cx = 0, cy = realToFrac winHeight / 2 * 0.8,
                                    lx = 0, ly = 0, laserFlying = False}
        signal = foldp newState initialState (lift2 combine' Keyboard.arrows Keyboard.space)
        combine' :: (Int, Int) -> Bool -> ((Int, Int), Bool)
        combine' (dx, dy) fired = ((dx, dy), fired)
        newState :: ((Int, Int), Bool) -> CannonState -> CannonState
        newState ((dx, _), fired) state = state {cx = cx',
                                                 lx = lx', ly = ly', laserFlying = laserFlying'}
          where cx' = cx state + realToFrac dx
                laserFlying' = (fired && ly state == 0) || (ly state /= 0 && ly state < 300)
                lx' | laserFlying' = lx state
                    | otherwise = cx'
                ly' = if laserFlying' then ly state + 1 else 0

cannonForm :: CannonState -> Form
cannonForm state = move (cx state, cy state) $ filled red $ square 64

laserForm :: CannonState -> Form
laserForm cannonState = move (lx', ly') $ filled white $ rect 2 10
  where lx' = lx cannonState
        ly' = cy cannonState - 30 - ly cannonState

render :: CannonState -> (Int, Int) -> Element
render cannonState (w, h) = centeredCollage w h [cannonForm cannonState,
                                                 laserForm cannonState]

main :: IO ()
main = do
  run config $ render <~ (cannonSignal winHeight) ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "space_invaders",
                                windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

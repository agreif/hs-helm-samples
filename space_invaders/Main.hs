import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import FRP.Elerea.Simple

data CannonState = CannonState {cx :: Double, cy :: Double,
                                lx :: Double, ly :: Double, laserFlying :: Bool}

data InvaderState = InvaderState {ix :: Double, iy :: Double, stepsX :: Int}


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

invaderSignal :: SignalGen(Signal InvaderState)
invaderSignal = signal
  where initialState = InvaderState {ix = -100, iy = -100, stepsX = -2}
        signal = foldp newState initialState count
        newState :: Int -> InvaderState -> InvaderState
        newState sampleCount state = state {ix = ix', iy = iy',
                                            stepsX = stepsX'}
          where ix' = ix state + dx
                iy' = iy state + dy
                sleep = 150
                stepNow = sampleCount `mod` (sleep) == 0
                steps = 6
                (dhori, dverti) = (20, 20)
                stepsDoublePlus1 = steps * 2 + 1
                stepsX' | stepNow && stepsX state == stepsDoublePlus1 = 0
                        | stepNow = stepsX state + 1
                        | otherwise = stepsX state
                (dx, dy) | stepNow && (stepsX' == steps || stepsX' == stepsDoublePlus1) = (0, dverti)
                         | stepNow && stepsX' < steps = (dhori, 0)
                         | stepNow && stepsX' > steps = (-dhori, 0)
                         | otherwise = (0, 0)

cannonForm :: CannonState -> Form
cannonForm state = move (cx state, cy state) $ filled red $ rect 64 32

laserForm :: CannonState -> Form
laserForm cannonState = move (lx', ly') $ filled white $ rect 2 10
  where lx' = lx cannonState
        ly' = cy cannonState - 15 - ly cannonState

invaderForm :: InvaderState -> Form
invaderForm invaderState = move (ix invaderState, iy invaderState) $ filled white $ rect 20 20

render :: CannonState -> InvaderState -> (Int, Int) -> Element
render cannonState invaderState (w, h) =
  centeredCollage w h $ [cannonForm cannonState,
                         laserForm cannonState,
                         invaderForm invaderState]

main :: IO ()
main = do
  run config $ render <~ (cannonSignal winHeight) ~~ invaderSignal ~~ Window.dimensions
  where config = defaultConfig {windowTitle = "space_invaders",
                                windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

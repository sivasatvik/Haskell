--Group 14 - Haskell Project

import Data.List
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


window :: Display
window = InWindow "Outer Space Gravity Simulation" (1200, 800) (50, 50)

background :: Color
background = black

data Object = Object { mass :: Float,
                       radius :: Float,
                       position_X :: Float,
                       position_Y :: Float,
                       velocity_X :: Float,
                       velocity_Y :: Float,
                       colour :: Color
                     } deriving (Show)

fps :: Int
fps = 20000

data SimState = SimState
  { obj1 :: Object,
    obj2 :: Object
  } deriving (Show)


earth  = Object 1     30   0 0 0 0      blue
moon   = Object 0.012  5  60 0 0 1000   white


same1  = Object 1      4  (-25) 0 0 (-20) red
same2  = Object 1      4    25  0 0   20  green
circXL = Object 1      70    0  0 0    0  (dark $ dark $ dark green)
circS  = Object 0.0001 10  100  0 0   20  (light $ light $ light green)
elliXL = Object 1      70    0  0 0    0  (dark $ dark $ dark red)
elliS  = Object 0.0001 10  100  0 0   22  (light $ light $ light red)
escaXL = Object 1      70    0  0 0    0  (dark $ dark $ dark blue)
escaS  = Object 0.0001 10  100  0 0   32  (light $ light $ light blue)
cold1  = Object 1      10 (-40) 0 0    0  cyan
cold2  = Object 1      10   40  0 0    0  (dark blue)
cust1  = Object 1      10 (-40) 0 0  (-5) cyan
cust2  = Object 1      10   40  0 0    5  (dark blue)




emStart  = SimState earth moon
smStart  = SimState same1 same2
coStart  = SimState circXL circS
eoStart  = SimState elliXL elliS
escStart = SimState escaXL escaS
colStart = SimState cold1 cold2
cusStart = SimState cust1 cust2


emRender :: SimState -> Picture
emRender simstate = pictures [translate ((position_X (obj1 simstate)) * 5) (position_Y (obj1 simstate) * 5) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 5,
                              translate ((position_X (obj2 simstate)) * 5) (position_Y (obj2 simstate) * 5) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 5
                             ]

eoRender :: SimState -> Picture
eoRender simstate = pictures [translate (-53.16) 0 $ scale 253.16 247.5 $ color violet $ circle $ 1,
                              translate ((position_X (obj1 simstate)) * 2) (position_Y (obj1 simstate) * 2) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 2,
                              translate ((position_X (obj2 simstate)) * 2) (position_Y (obj2 simstate) * 2) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 2
                             ]

coRender :: SimState -> Picture
coRender simstate = pictures [color violet $ circle $ 200,
                              translate ((position_X (obj1 simstate)) * 2) (position_Y (obj1 simstate) * 2) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 2,
                              translate ((position_X (obj2 simstate)) * 2) (position_Y (obj2 simstate) * 2) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 2
                             ]

escRender :: SimState -> Picture
escRender simstate = pictures [translate ((position_X (obj1 simstate)) * 2) (position_Y (obj1 simstate) * 2) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 2,
                               translate ((position_X (obj2 simstate)) * 2) (position_Y (obj2 simstate) * 2) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 2
                              ]


smRender :: SimState -> Picture
smRender simstate = pictures [color violet $ circle $ 250,
                              translate ((position_X (obj1 simstate)) * 10) (position_Y (obj1 simstate) * 10) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 10,
                              translate ((position_X (obj2 simstate)) * 10) (position_Y (obj2 simstate) * 10) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 10
                             ]


colRender :: SimState -> Picture
colRender simstate = pictures [translate ((position_X (obj1 simstate)) * 10) (position_Y (obj1 simstate) * 10) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 10,
                               translate ((position_X (obj2 simstate)) * 10) (position_Y (obj2 simstate) * 10) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 10
                              ]

cusRender :: SimState -> Picture
cusRender simstate = pictures [translate ((position_X (obj1 simstate)) * 10) (position_Y (obj1 simstate) * 10) $ color (colour (obj1 simstate)) $ circleSolid $ (radius (obj1 simstate)) * 10,
                               translate ((position_X (obj2 simstate)) * 10) (position_Y (obj2 simstate) * 10) $ color (colour (obj2 simstate)) $ circleSolid $ (radius (obj2 simstate)) * 10
                              ]


realisticCompute :: Float -> SimState -> SimState
realisticCompute seconds state = new_state
  where
    x1  = position_X $ obj1 state
    x2  = position_X $ obj2 state
    y1  = position_Y $ obj1 state
    y2  = position_Y $ obj2 state
    m1  = mass $ obj1 state
    m2  = mass $ obj2 state
    vx1 = velocity_X $ obj1 state
    vy1 = velocity_Y $ obj1 state
    vx2 = velocity_X $ obj2 state
    vy2 = velocity_Y $ obj2 state

    period = seconds * 86400

    mass1 = m1 * (5.972 * 10^(24))
    mass2 = m2 * (5.972 * 10^(24))

    gravity_constant = 6.67408 * (10^^(-11))

    distance = (((x2 - x1)^2 + (y2 - y1)^2) ** (0.5)) * 6371000

    force = (gravity_constant * mass1 * mass2) / (distance ^ 2)

    force_X1 = force * (((x2 - x1)*6371000)/distance)

    force_Y1 = force * (((y2 - y1)*6371000)/distance)

    force_X2 = (-1) * force_X1

    force_Y2 = (-1) * force_Y1

    i_new_X1 = (vx1 * period) + ((0.5) * (period)^2 * (force_X1/mass1))
    i_new_Y1 = (vy1 * period) + ((0.5) * (period)^2 * (force_Y1/mass1))
    i_new_X2 = (vx2 * period) + ((0.5) * (period)^2 * (force_X2/mass2))
    i_new_Y2 = (vy2 * period) + ((0.5) * (period)^2 * (force_Y2/mass2))

    displacement_correction = 6371000

    new_X1 = (i_new_X1 / displacement_correction) + x1
    new_Y1 = (i_new_Y1 / displacement_correction) + y1
    new_X2 = (i_new_X2 / displacement_correction) + x2
    new_Y2 = (i_new_Y2 / displacement_correction) + y2

    new_vX1 = vx1 + ((period) * (force_X1/mass1))
    new_vY1 = vy1 + ((period) * (force_Y1/mass1))

    new_vX2 = vx2 + ((period) * (force_X2/mass2))
    new_vY2 = vy2 + ((period) * (force_Y2/mass2))

    newObj1 = Object m1 (radius $ obj1 state) new_X1 new_Y1 new_vX1 new_vY1 (colour $ obj1 state)
    newObj2 = Object m2 (radius $ obj2 state) new_X2 new_Y2 new_vX2 new_vY2 (colour $ obj2 state)

    new_state = SimState newObj1 newObj2




pseudoCompute :: Float -> SimState -> SimState
pseudoCompute seconds state = new_state
  where
    x1 = position_X $ obj1 state
    x2 = position_X $ obj2 state
    y1 = position_Y $ obj1 state
    y2 = position_Y $ obj2 state
    m1 = mass $ obj1 state
    m2 = mass $ obj2 state
    vx1 = velocity_X $ obj1 state
    vy1 = velocity_Y $ obj1 state
    vx2 = velocity_X $ obj2 state
    vy2 = velocity_Y $ obj2 state

    period = seconds

    gravity_constant = 40000

    distance = ((x2 - x1)^2 + (y2 - y1)^2) ** (0.5)

    force = (gravity_constant * m1 * m2) / (distance ^ 2)

    force_X1 = force * ((x2 - x1)/distance)

    force_Y1 = force * ((y2 - y1)/distance)

    force_X2 = (-1) * force_X1

    force_Y2 = (-1) * force_Y1

    new_X1 = x1 + (vx1 * period) + ((0.5) * (period)^2 * (force_X1/m1))
    new_Y1 = y1 + (vy1 * period) + ((0.5) * (period)^2 * (force_Y1/m1))

    new_X2 = x2 + (vx2 * period) + ((0.5) * (period)^2 * (force_X2/m2))
    new_Y2 = y2 + (vy2 * period) + ((0.5) * (period)^2 * (force_Y2/m2))


    new_vX1 = vx1 + ((period) * (force_X1/m1))
    new_vY1 = vy1 + ((period) * (force_Y1/m1))

    new_vX2 = vx2 + ((period) * (force_X2/m2))
    new_vY2 = vy2 + ((period) * (force_Y2/m2))

    newObj1 = Object m1 (radius $ obj1 state) new_X1 new_Y1 new_vX1 new_vY1 (colour $ obj1 state)
    newObj2 = Object m2 (radius $ obj2 state) new_X2 new_Y2 new_vX2 new_vY2 (colour $ obj2 state)

    new_state = SimState newObj1 newObj2



main :: IO ()
main = do

  putStrLn "\n----------------------------------------\n--- 2D OUTER SPACE GRAVITY SIMULATOR ---"
  putStrLn "----------------------------------------\n \nPlease select one of the following pre-defined simulations: \n"
  putStrLn "(1) Earth - Moon Simulation\n(2) Same Mass Cyclic Simulation"
  putStrLn "(3) Circular Orbit Simulation\n(4) Elliptical Orbit Simulation"
  putStrLn "(5) Escape Orbit Simulation\n(6) Collision Simulation\n(7) Collision Simulation\n-- Hit Escape during Simulation to exit --\n\nInput: "
  input <- getLine
  let number = (read input :: Int)

  case number of
    1 -> simulate window background fps emStart emRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = realisticCompute
    2 -> simulate window background fps smStart smRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    3 -> simulate window background fps coStart coRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    4 -> simulate window background fps eoStart eoRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    5 -> simulate window background fps escStart escRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    6 -> simulate window background fps colStart colRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    7 -> simulate window background fps cusStart cusRender update
           where
             update :: ViewPort -> Float -> SimState -> SimState
             update _ = pseudoCompute
    otherwise -> putStrLn "Improper Input! Try Again!"

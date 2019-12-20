module Solve.Day12 where

import Data.List ((\\), findIndex)
import Data.Maybe (fromJust)

-- <x=-4, y=3, z=15>
-- <x=-11, y=-10, z=13>
-- <x=2, y=2, z=18>
-- <x=7, y=-1, z=0>

input = [ Moon { name = "Io", position = (-4, 3, 15), velocity = (0, 0, 0) }
        , Moon { name = "Europa", position = (-11, -10, 13), velocity = (0, 0, 0) }
        , Moon { name = "Ganymede", position = (2, 2, 18), velocity = (0, 0, 0) }
        , Moon { name = "Callisto", position = (7, -1, 0), velocity = (0, 0, 0) }
        ]

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

data Moon = Moon
  { name :: String
  , position :: Position
  , velocity :: Velocity
  } deriving (Show)

instance Eq Moon where
  (==) (Moon { name = nameA }) (Moon { name = nameB }) = nameA == nameB

addTriple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTriple (x1, y1, z1) (x2, y2, z2) = (x, y, z)
  where 
    x = x1 + x2
    y = y1 + y2
    z = z1 + z2

applyGravity :: Position -> Position -> Velocity
applyGravity (pxA, pyA, pzA) (pxB, pyB, pzB) = 
  (getGravity pxA pxB, getGravity pyA pyB, getGravity pzA pzB)
  where
    getGravity positionA positionB
      | positionA < positionB = 1
      | positionA > positionB = -1
      | otherwise = 0

potentialEnergy :: Moon -> Int
potentialEnergy (Moon { position }) = calculateEnergy position

kineticEnergy :: Moon -> Int
kineticEnergy (Moon { velocity }) = calculateEnergy velocity

calculateEnergy :: (Int, Int, Int) -> Int
calculateEnergy (x, y, z) = abs x  + abs y + abs z

totalEnergy :: [Moon] -> Int
totalEnergy = foldr sumEnergy 0
  where
    sumEnergy moon energy = energy + (kineticEnergy moon * potentialEnergy moon)

timeStep :: [Moon] -> [Moon]
timeStep moons = map stepMoon moons
  where 
    stepMoon moon@(Moon { position, velocity }) =
      let otherMoons = moons \\ [moon]
          velocityDiff = foldr addTriple (0,0,0) [applyGravity position otherPosition | (Moon { position = otherPosition }) <- otherMoons]
          newVelocity = addTriple velocity velocityDiff
          newPosition = addTriple position newVelocity
      in moon 
        { velocity = newVelocity
        , position = newPosition
        }

partOne :: Int -> [Moon] -> Int
partOne n moons = totalEnergy $ iterate timeStep moons !! n

partTwo :: [Moon] -> Int
partTwo moons = lcm cyclesTilInitialX $ lcm cyclesTilInitialY cyclesTilInitialZ
  where
    initialAx = isInitialX (moons !! 0)
    initialBx = isInitialX (moons !! 1)
    initialCx = isInitialX (moons !! 2)
    initialDx = isInitialX (moons !! 3)
    initialAy = isInitialY (moons !! 0)
    initialBy = isInitialY (moons !! 1)
    initialCy = isInitialY (moons !! 2)
    initialDy = isInitialY (moons !! 3)
    initialAz = isInitialZ (moons !! 0)
    initialBz = isInitialZ (moons !! 1)
    initialCz = isInitialZ (moons !! 2)
    initialDz = isInitialZ (moons !! 3)
    initialX [a,b,c,d] = initialAx a && initialBx b && initialCx c && initialDx d
    initialY [a,b,c,d] = initialAy a && initialBy b && initialCy c && initialDy d
    initialZ [a,b,c,d] = initialAz a && initialBz b && initialCz c && initialDz d
    cyclesTilInitialX = (+) 1 $ fromJust $ findIndex initialX $ iterate timeStep $ timeStep moons
    cyclesTilInitialY = (+) 1 $ fromJust $ findIndex initialY $ iterate timeStep $ timeStep moons
    cyclesTilInitialZ = (+) 1 $ fromJust $ findIndex initialZ $ iterate timeStep $ timeStep moons

isInitialX :: Moon -> Moon -> Bool
isInitialX initial moon = px == value && vx == 0
  where
    (value, _, _) = position initial
    (px, _, _) = position moon
    (vx, _, _) = velocity moon

isInitialY :: Moon -> Moon -> Bool
isInitialY initial moon = py == value && vy == 0
  where
    (_, value, _) = position initial
    (_, py, _) = position moon
    (_, vy, _) = velocity moon

isInitialZ :: Moon -> Moon -> Bool
isInitialZ initial moon = pz == value && vz == 0
  where
    (_, _, value) = position initial
    (_, _, pz) = position moon
    (_, _, vz) = velocity moon


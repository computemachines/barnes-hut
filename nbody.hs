-- implementation of barnes-hut nbody algorithm

dt = 1.0

data Vector = Vector Double Double deriving (Eq, Show)

instance Num Vector where
  Vector x y + Vector i j = Vector (x+i) (y+j)
  Vector x y * Vector i j = Vector (x*i) (y*j)
Vector x y `multScalar` c = Vector (x*c) (y*c)
Vector x y `divScalar` c = Vector (x/c) (y/c)

--  abs (Vector x y) = Vector abs x abs y
--  signum (Vector x y) = Vector 0::Double 0::Double
data Region = Rect { center :: Vector, radius :: Double }

data Particle = Particle{ mass :: Double,
                          position :: Vector,
                          velocity :: Vector }
                | PseudoParticle { mass :: Double,
                                   position :: Vector }
              deriving (Show)

instance Num Particle where
  PseudoParticle m1 p1 + PseudoParticle m2 p2 =
    PseudoParticle (m1+m2) (p1+p2)

average :: [Particle] -> Particle
average [] = error ""
average [particle] = particle
average particles 
  = PseudoParticle avmass avpos
  where
    tmass = (sum (map mass particles))
    weightedPosition particle
      = (position particle) `multScalar` (mass particle)
    avpos = (foldl1 (+) (map weightedPosition particles)) `divScalar` tmass
    avmass = tmass / (fromIntegral $ length particles)
  

data Node = Leaf Particle
          | Empty
          | Node { children :: [Node] }
          deriving (Show)

pseudo :: Particle -> Particle
pseudo (Particle m pos _) = PseudoParticle m pos

centerOfMass :: Node -> Particle
centerOfMass Empty = PseudoParticle 0 $ Vector 0 0
centerOfMass (Leaf particle) = pseudo particle
centerOfMass (Node children)
  = average (map centerOfMass children)
--centerOfMass (Leaf particle) = 

stepParticle :: Particle -> Vector -> Particle
stepParticle (Particle { mass = m, position = pos, velocity = vel }) force
  = Particle { mass = m,
               position = pos + vel `multScalar` dt,
               velocity = vel + acc `multScalar` dt }
  where
    acc = force `divScalar` m

constructNode :: Region -> [Particle] -> Node
constructNode rect [particle] = Leaf particle

vector = Vector 1 2
particle = Particle 1 vector vector
node = Node [Leaf particle, Leaf $ Particle 1 (Vector 100 0) vector]

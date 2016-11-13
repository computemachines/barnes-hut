module NBody (
  Vector,
  Region,
  ) where

-- implementation of barnes-hut nbody algorithm

dt = 1.0

data Vector = Vector Double Double deriving (Eq, Show)

instance Num Vector where
  Vector x y + Vector i j = Vector (x+i) (y+j)
  Vector x y * Vector i j = Vector (x*i) (y*j)
Vector x y `multScalar` c = Vector (x*c) (y*c)
Vector x y `divScalar` c = Vector (x/c) (y/c)

data Region = Square { center :: Vector, radius :: Double } deriving (Show)
isContainedBy :: Vector -> Region -> Bool
(Vector x y) `isContainedBy` (Square (Vector cx cy) r) 
  = and [abs (x-cx) < r, abs (y-cy) < r]

contains :: Region -> Vector -> Bool
r `contains` v = v `isContainedBy` r
    
partition :: Region -> [Region]
partition (Square (Vector x y) r)
  = [Square (Vector (x+i) (y+j)) (r/2)
    | i <- flipflop, j <- flipflop]
    where flipflop = [-r/2,r/2]

class PseudoParticle a where
  mass :: a -> Double
  position :: a -> Vector

data Particle = Particle Double Vector Vector deriving (Show)

instance PseudoParticle Particle where
  mass (Particle m _ _) = m
  position (Particle _ p _) = p

velocity :: Particle -> Vector
velocity (Particle _ _ v) = v

data Node = Node { region :: Region, children :: Either [Node] Particle }
            deriving (Show)

-- pseudoparticle node represents total mass and center of mass
instance PseudoParticle Node where
  mass (Node _ (Left children)) = sum $ map mass children
  mass (Node _ (Right particle)) = mass particle

  position (Node _ (Right particle)) = position particle
  position node@(Node _ (Left children))
    = foldl1 (+) [weightedPosition pseudoParticle
                 | pseudoParticle <- children]
    where
      weightedPosition particle
        = (position particle) `multScalar` ((mass particle)/(mass node))

stepParticle :: Particle -> Vector -> Particle
stepParticle (Particle m pos vel) force
  = Particle m (pos + vel `multScalar` dt) (vel + acc `multScalar` dt)
  where
    acc = force `divScalar` m

emptyNode region = Node region $ Left []

insertInto :: Particle -> Node -> Node
particle `insertInto` (Node region (Left [])) = Node region (Right particle)
newParticle `insertInto` self@(Node region (Right particle))
  | position newParticle == position particle = self
  | otherwise = insertInto newParticle $ insertInto particle node
  where
    node = Node region $ Left (map emptyNode $ partition region)

-- return new node with correct child replaced with
--                                   (insertInto particle child)
--particle `insertInto` self@(Node region (Left children))
--  = Node region $ Left [head $ filter ((isContainedBy $ position particle) . region) children]

constructNode :: Region -> [Particle] -> Node
constructNode region [particle] = Node region $ Right particle
constructNode region particles = Node region $ Left (
  [constructNode subregion containedParticles
   | (subregion, containedParticles) <- sorted])
  where
    particlesIn :: Region -> [Particle]
    particlesIn r = filter ((contains r) . position) (particles)
    sorted = [(subr, (particlesIn subr)) | subr <- (partition region)]
--constructNode region (particle:particles)


vector = Vector 0 0
rect = Square vector 10
particle = Particle 1 vector vector
particle2 = Particle 2 (Vector (3) 4) vector
-- node = Node [Leaf particle, Leaf $ Particle 1 (Vector 100 0) vector]

root = emptyNode $ Square (Vector 0 0) 10
root2 = particle `insertInto` root
root3 = particle2 `insertInto` root2

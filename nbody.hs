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
partition :: Region -> [Region]
partition (Square (Vector x y) r)
  = [Square (Vector (x+i) (y+j)) (r/2)
    | i <- [-1,1], j <- [-1,1]]

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

insertInto :: Particle -> Node -> Node
particle `insertInto` (Node region (Left [])) = Node region (Right particle)
--newParticle `insertInto` (Node region (Right particle))
--  = Node region 

--particle `insertInto` (Node region (
--constructNode region [particle] = Leaf particle
--constructNode region (particle:particles)


vector = Vector 1 2
rect = Square vector 1
particle = Particle 1 vector vector
-- node = Node [Leaf particle, Leaf $ Particle 1 (Vector 100 0) vector]

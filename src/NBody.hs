module Body (
  ) where

-- Hierarchical collection of particles 

class ParticleLike a where
  mass :: a -> Double
  position :: a -> Vector

data Node = Node { region :: Region, children :: [Node] }
          | Particle Double Vector Vector
            deriving (Show)

-- node represents total mass and center of mass
instance ParticleLike Node where
  mass (Node _ []) = 0
  mass (Node _ children) = sum . map mass $ children
  
  position node@(Node _ children)
    = foldl1 (+) . map weightedPosition $ children
    where
      weightedPosition child
        = (position child) `multScalar` ((mass child)/(mass node))

-- stepParticle :: Particle -> Vector -> Particle
-- stepParticle (Particle m pos vel) force
--   = Particle m (pos + vel `multScalar` dt) (vel + acc `multScalar` dt)
--   where
--     acc = force `divScalar` m

-- emptyNode region = Node region $ Left []

-- insertInto :: Particle -> Node -> Node
-- particle `insertInto` (Node region (Left [])) = Node region (Right particle)
-- newParticle `insertInto` self@(Node region (Right particle))
--   | position newParticle == position particle = self
--   | otherwise = insertInto newParticle $ insertInto particle node
--   where
--     node = Node region $ Left (map emptyNode $ partition region)

-- -- return new node with correct child replaced with
-- --                                   (insertInto particle child)
-- --particle `insertInto` self@(Node region (Left children))
-- --  = Node region $ Left [head $ filter ((isContainedBy $ position particle) . region) children]

-- constructNode :: Region -> [Particle] -> Node
-- constructNode region [particle] = Node region $ Right particle
-- constructNode region particles = Node region $ Left (
--   [constructNode subregion containedParticles
--    | (subregion, containedParticles) <- sorted])
--   where
--     particlesIn :: Region -> [Particle]
--     particlesIn r = filter ((contains r) . position) (particles)
--     sorted = [(subr, (particlesIn subr)) | subr <- (partition region)]
-- --constructNode region (particle:particles)


-- vector = Vector 0 0
-- rect = Square vector 10
-- particle = Particle 1 vector vector
-- particle2 = Particle 2 (Vector (3) 4) vector
-- -- node = Node [Leaf particle, Leaf $ Particle 1 (Vector 100 0) vector]

-- root = emptyNode $ Square (Vector 0 0) 10
-- root2 = particle `insertInto` root
-- root3 = particle2 `insertInto` root2

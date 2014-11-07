-- implementation of barnes-hut nbody algorithm

dt = 1.0

type Vector = (Float, Float) 
type Rect = (Vector, Vector)

data Particle = Particle { mass :: Float,
                           position :: Vector,
                           velocity :: Vector }

data Node = Node Float Vector [Node]

-- stepParticle (Particle { mass = m, position = pos, velocity = vel }) force
--   = Particle { mass = m, position = pos + vel*dt, velocity = vel + acc*dt }
--   where
--     acc = force / m


                           

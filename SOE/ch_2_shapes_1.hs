import Data.List

type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

data Shape =   Rectangle  Side Side
             | Ellipse    Radius Radius
             | RtTriangle Side Side
             | Polygon    [Vertex]
               deriving Show

square   :: Side -> Shape
square s =  Rectangle s s

circle   :: Radius -> Shape
circle r =  Ellipse r r

{- Exercise 2.1 
Define functions rectangle and rtTriangle in terms of Polygon.
-}

rectangle s1 s2 = Polygon[(0,0), (s1, 0), (s1, s2), (0, s2)]

rtTriangle s1 s2 = Polygon[(0,0), (s1, 0), (0, s2)]

{- Exercise 2.2
Define a function regularPolygon :: Int → Side → Shape such that
regularPolygon n s is a regular polygon with n sides, each of length s.
(Hint: Consider using some of Haskell’s trigonometric functions, such as
sin :: Float → Float, cos :: Float → Float, and tan :: Float → Float.)
-}

regularPolygon n s = [(r * cos phi, r * sin phi) | k <- [0 .. (n-1)],
                                                   let r   = s / (2 * sin (pi/n))
                                                       phi = k * 2 * pi / n]

-- the above solution treats n as a Float, to give the correct type signature,
-- it is necessary to convert n from Int to Num using fromIntegral:

-- regularPolygon :: Int -> Side -> Shape
-- regularPolygon n s = [(r * cos phi, r * sin phi) | k <- [0 .. (n-1)],
--                                                    let pin = pi / fromIntegral n
--                                                        r = s / (2 * sin pin)
--                                                        phi = k * 2 * pin]

{- The above version fails with the error:
    Couldn't match expected type `Int' with actual type `Float'
    In the second argument of `(*)', namely `pin'
    In the expression: k * 2 * pin
    In an equation for `phi': phi = k * 2 * pin

Not been able to find the issue.

Solutions' version works:
-}
regularPolygon' :: Int -> Side -> Shape
regularPolygon' n s
    = let angleinc = pi * 2 / fromIntegral n
          radius = s * sin ((pi - angleinc) / 2) / sin angleinc
          regularVerts 0 _ = []
          regularVerts n angle = (radius * cos angle, radius * sin angle)
                                 : regularVerts (n-1) (angle + angleinc)
      in Polygon (regularVerts n 0)

-- § 2.2  Areas of Shapes

distBetween                   :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) =  sqrt ( (x2 - x1)^2 +
                                        (y2 - y1)^2 )

triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = 0.5 * (a + b + c)
                   in  sqrt (s * (s - a) * (s - b) * (s - c))

area                    :: Shape -> Float
area (Rectangle s1 s2)  =  s1 * s2
area (RtTriangle s1 s2) =  s1 * s2 / 2
area (Ellipse r1 r2)    =  pi * r1 * r2
area (Polygon (v1:vs))  = polyArea vs
                            where polyArea             :: [Vertex] -> Float
                                  polyArea (v2:v3:vs') =  triArea v1 v2 v3
                                                          + polyArea (v3:vs')
                                  polyArea _           =  0


vexPoly = Polygon [(3.0,4.0), (3.0,2.0), (1.0,2.0), (1.0,4.0)]
cavePoly = Polygon [(1,1), (3,4), (4,1), (3,2)]
triPoly = Polygon [(3.0,4.0), (3.0,2.0), (1.0,2.0)]

rotate [] = []
rotate xs = tail xs ++ [head xs]

orderVertices (Polygon vs) = minimum (take (length vs) (iterate rotate vs))

closeLoop xs = xs ++ [head xs]

-- define some helper functions

getVertices              :: Shape -> [Vertex]
getVertices (Polygon vs) =  vs ++ [head vs]

-- vecs turns an ordered list of vertices into an order list of vectors.
vecs                       :: (Num t, Num t1) => [(t, t1)] -> [(t, t1)]
vecs [x]                   =  []
vecs ((x1,y1):(x2,y2):xys) =  (x2-x1, y2-y1): vecs ((x2,y2):xys)

crossProduct                       :: Num t => [(t,t)] -> [t]
crossProduct [x]                   =  []
crossProduct ((x1,y1):(x2,y2):xys) =  (x1*y2-x2*y1): crossProduct ((x2,y2):xys)

isConvex' poly | all (< 0) xs = True
               | all (> 0) xs = True
               | otherwise    = False
                 where     xs = (crossProduct . vecs . getVertices) poly

-- Test data

t1 = [(1,2),(1,4),(3,4)]         -- Triangle
t2 = [(1,2),(1,4),(3,4),(3,2)]   -- Convex  Quadrilateral
t3 = [(1,2),(1,4),(3,4),(5,4)]   -- Convex  Quadrilateral (degenerate)
t4 = [(1,1),(3,4),(4,1),(3,2)]   -- Concave Quadrilateral
t5 = [(3, 0), (1, 1), (0, 4), (3, 6), (4, 4)] -- Convex Pentgram
t6 = [(3, 0), (3, 1), (0, 4), (3, 6), (4, 4)] -- Concave Pentgram
t7 = [(3, 0), (3, 1), (0, 4), (3, 4), (4, 4)] -- Concave Pentgram (degenerate)

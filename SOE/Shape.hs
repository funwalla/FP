module Shape (Shape (Rectangle, Ellipse, RtTriangle, Polygon),
              Radius, Side, Vertex,
              square, circle, distBetween,
              area, areaPoly, signedArea, getVertices
             ) where

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

{- Exercise 2.4  Define a function isConvex :: Shape -> Bool that
determines whether or not its argument is a convex shape.

I've defined isConvex to work on polygons using the property that the
cross-products of the vectors defined by adjacent sides will all have
the same sign for convex polygons, but will have different signs for
concave polygons. The degenerate case of 3 or more collinear points is
allowed devolve into a polygon with fewer sides.
-}

-- define some helper functions

getVertices              :: Shape -> [Vertex]
getVertices (Polygon vs) =  vs ++ [head vs]

-- getVectors turns an ordered list of vertices into an order list of vectors.
getVectors                       :: (Num t, Num t1) => [(t, t1)] -> [(t, t1)]
getVectors [x]                   =  []
getVectors ((x1,y1):(x2,y2):xys) =  (x2-x1, y2-y1): getVectors ((x2,y2):xys)

-- compute the 2-D cross-product
crossProduct                       :: Num t => [(t,t)] -> [t]
crossProduct [x]                   =  []
crossProduct ((x1,y1):(x2,y2):xys) =  (x1*y2 - x2*y1): crossProduct ((x2,y2):xys)

isConvex                      :: Shape -> Bool
isConvex poly | all (<= 0) xs =  True
              | all (>= 0) xs =  True
              | otherwise     =  False
                where     xs  =  (crossProduct . getVectors . getVertices) poly

-- Testing:

tstPoly = map Polygon
              [[(1,2),(1,4),(3,4)],         -- Triangle
               [(1,2),(1,4),(3,4),(3,2)],   -- Convex  Quadrilateral
               [(1,2),(1,4),(3,4),(5,4)],   -- Convex  Quadrilateral (degenerate)
               [(1,1),(3,4),(4,1),(3,2)],   -- Concave Quadrilateral
               [(3, 0), (1, 1), (0, 4), (3, 6), (4, 4)], -- Convex Pent.
               [(3, 0), (3, 1), (0, 4), (3, 6), (4, 4)], -- Concave Pent.
               [(3, 0), (1, 1), (0, 4), (3, 4), (4, 4)]] -- Concave Pent. (degen)

expectedResults = [True,True,True,False,True,False,True]
actualResults = map isConvex tstPoly

tstResults :: [Bool] -> [Bool] -> [Char]
tstResults actual expected | actual == expected = "All tests passed."
                           | otherwise          = "Some tests failed."

results = tstResults actualResults expectedResults


{- Exercise 2.5
    Consider a polygon in quadrant 1 of the Cartesian plane
    (i.e., every vertex has positive x and y coordinates).
    Then every pair of adjacent vertices forms a trapeziod
    with respect to the x-axis. Starting at any vertex and
    working clockwise, compute these areas one-by-one, counting
    the area as positive if the x-coordinate increases, and
    negative if it decreases. The sum of these areas is then
    the area of the polygon.
-}

-- areaPoly essentially computes the area twice since the helper function
-- isClockwise uses the signedArea function. The only way I see to get
-- around this is to require that the argument polygon, poly, present its
-- vertices in clockwise order.

areaPoly      :: Shape -> Float
areaPoly poly =  trapArea vs
                 where vs | isClockwise poly = getVertices poly
                          | otherwise        = reverse (getVertices poly)

-- helper functions
trapArea            :: Fractional a => [(a, a)] -> a
trapArea [p]        =  0
trapArea (p1:p2:ps) =  trap p1 p2 + trapArea (p2:ps)
                       where trap (x1,y1) (x2,y2) = 0.5 * (x2-x1) * (y1 + y2)

isClockwise                   :: Shape -> Bool
isClockwise poly | sign <= 0  =  True
                 | sign >  0  =  False
                   where sign = (signedArea . getVertices) poly

signedArea                      :: Fractional a => [(a, a)] -> a
signedArea [x]                  =  0
signedArea ((x1,y1):(x2,y2):xs)  = 0.5 * (x1*y2 - x2*y1) +
                                         signedArea ((x2,y2):xs)

-- define some tst data:

tst1 = Polygon [(1, 0), (2, 4), (3, 0)]  -- clockwise triangle
tst2 = Polygon [(1, 0), (3, 0), (2, 4)]  -- anticlockwise triangle
tst3 = Polygon [(3, 0), (3, 1), (0, 4), (3, 6), (4, 4)] -- concave pentagram
tst4 = Polygon [(1,1), (1,3), (2,2), (3,3), (3,1), (2,2)] -- "bow tie"

trap' (x1,y1) (x2,y2) = 0.5 * (x2-x1) * (y1 + y2)

tstTraps [v] = []
tstTraps (v1:v2:vs) = trap' v1 v2 : tstTraps (v2:vs)







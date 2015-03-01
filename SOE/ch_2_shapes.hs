
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
             deriving Show

-- Exercise 2.1

rectangle     :: Float -> Float -> Shape
rectangle w l =  Polygon [(0, 0), (w, 0), (w, l), (0, l)]

rtTriangle     :: Float -> Float -> Shape
rtTriangle w l =  Polygon [(0, 0), (w, 0), (0, l)]

-- Exercise 2.2

-- Using geometric results from http://www.math.rutgers.edu/~erowland/polygons.html
-- r = radius of circumscribed circle = s / (2 * sin (pi/n))

regularPolygon n s = [(r * cos theta, r * sin theta) | theta <- thetas ]
    where r      = s / (2 * sin (pi/n))
          thetas = map (* r) [0 .. (n-1)]

regularPolygon2 :: Int -> Side -> Shape
regularPolygon2 n s = Polygon (map makeVertex [1 .. n])
    where makeVertex i = (radius * cos (angle i), radius * sin (angle i))
          radius = s / (2 * sin halfExterior)
          angle i = fromIntegral (2 * i) * halfExterior
          halfExterior = pi / fromIntegral n

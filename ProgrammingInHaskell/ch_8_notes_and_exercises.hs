-- Chapter 8  Functional Parsers

--  8.1 Parsers

-- 8.2 The parser type

type Parser a = String -> [(a, String)]

-- 8.3 Basic parsers

return'   :: a -> Parser a
return' v =  \ inp -> [(v, inp)]

failure :: Parser a
failure =  \ inp -> []

item :: Parser Char
item =  \ inp -> case inp of
                   []       -> []
                   (x : xs) -> [(x, xs)]

parse       :: Parser a -> String -> [(a, String)]
parse p inp =  p inp

-- 8.4 Sequencing

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =  \ inp -> case parse p inp of
                      [] -> []
                      [(v, out)] -> parse (f v) out

-- p :: Parser (Char, Char)
-- p =  do x <- item
--         z <- item
--         y <- item
--         return' (x, y)

-- -- 8.5 Choice

-- (+++) :: Parser a -> Parser a -> Parser a
-- p +++ q =  \ inp -> case p inp of
--                       []         -> parse q inp
--                       [(v, out)] -> [(v, out)]

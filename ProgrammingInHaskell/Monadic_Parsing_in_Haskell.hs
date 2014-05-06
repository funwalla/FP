
-- 2 A type for parsers

newtype Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item =  Parser (\ cs -> case cs of
                          "" -> []
                          (c : cs) -> [(c, cs)])
parse (Parser p) = p

instance Monad Parser where
  return a = Parser (\ cs -> [(a, cs)])
  p >>= f  = Parser (\ cs -> concat [parse (f a) cs' |
                                     (a, cs') <- parse p cs])

  

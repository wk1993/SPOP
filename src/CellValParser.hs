module CellValParser (evalFromString) where

import Model
import Data.Char

-- evaluates user-provided string and tries to create CellVal from it
evalFromString :: String -> CellVal
evalFromString inp = case parse parseFromString inp of
              [(n, [])] -> case n of
                               SumFunc _ _ -> n { str = inp }
                               MulFunc _ _ -> n { str = inp }
                               AvgFunc _ _ -> n { str = inp }
                               _           -> n
              [(_, out)] -> error ("Error during parsing, unconsumed: " ++ out)
              [] -> error "Wrong input"

parseFromString :: Parser CellVal
parseFromString = do
                        char '='
                        char 's'
                        char 'u'
                        char 'm'
                        char '('
                        r <- rangeList
                        char ')'
                        return (SumFunc r "")
                    +++
                    do
                        char '='
                        char 'm'
                        char 'u'
                        char 'l'
                        char '('
                        r <- rangeList
                        char ')'
                        return (MulFunc r "")
                    +++
                    do
                        char '='
                        char 'a'
                        char 'v'
                        char 'g'
                        char '('
                        r <- rangeList
                        char ')'
                        return (AvgFunc r "")
                    +++
                    do
                        n <- number
                        return (NumVal n)
                    +++
                    do
                        s <- string
                        return (StringVal s)


rangeList :: Parser [(Char, Int)]
rangeList = do
                c <- listElement
                do
                    char ';'
                    nx <- rangeList
                    return (c ++ nx)
                    +++
                        return c

listElement :: Parser [(Char, Int)]
listElement = do
                  colb <- sat (isAlpha)
                  rowb <- many digit
                  char ':'
                  cole <- sat (isAlpha)
                  rowe <- many digit
                  return (generateRange colb (read rowb) cole (read rowe))
                  +++
                  do
                      col <- sat (isAlpha)
                      row <- many digit
                      return [(col, read row)]

generateRange :: Char -> Int -> Char -> Int -> [(Char, Int)]
generateRange cb rb ce re = do
                                if re > rb then 
                                    if ce > cb then
                                        [(c, r) | c <- [cb .. ce], r <- [rb .. re]]
                                    else 
                                        [(c, r) | c <- [ce .. cb], r <- [rb .. re]]
                                else 
                                    if ce > cb then
                                        [(c, r) | c <- [cb .. ce], r <- [re .. rb]]
                                    else 
                                        [(c, r) | c <- [ce .. cb], r <- [re .. rb]]

number :: Parser Double
number = P (\inp -> case reads inp :: [(Double, String)] of
                        [] -> []
                        [(n, "")] -> [(n, "")]
                        [(_, _)] -> [])

string :: Parser String
string = P (\inp -> [(inp, "")])

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f m = m >>= pure . f

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Parser where
  return = pure
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

failure :: Parser a
failure = P (\inp -> [])

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
            x <- item
            if p x then return x else failure

char :: Char -> Parser Char
char x = sat (\c -> (toLower c) == x)

digit :: Parser Char
digit = sat isDigit

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

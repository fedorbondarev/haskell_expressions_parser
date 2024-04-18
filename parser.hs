{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Char (isDigit, isSpace, ord)
import Data.Foldable (foldl')
import GHC.Unicode (isAsciiUpper)

newtype Parser s a
  = Parser {runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
  fmap f (Parser p) = Parser $ \s -> do
    (a, s') <- p s
    return (f a, s')

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  Parser p <*> Parser q = Parser $ \s -> do
    (f, s') <- p s
    (a, s'') <- q s'
    return (f a, s'')

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser p <|> Parser q = Parser $ \ss -> case p ss of
    Nothing -> q ss
    res -> res

instance Monad (Parser s) where
  return = pure
  Parser p >>= f = Parser $ \s -> do
    (a, s') <- p s
    runParser (f a) s'

ok :: Parser s ()
ok = pure ()

eof :: Parser s ()
eof = Parser $ \s ->
  if null s then Just ((), []) else Nothing

parseHead :: Parser s s
parseHead = Parser $ \case
  (s : ss) -> Just (s, ss)
  _ -> Nothing

satisfies :: (s -> Bool) -> Parser s s
satisfies p = Parser $ \case
  (s : ss) | p s -> Just (s, ss)
  _ -> Nothing

element :: (Eq s) => s -> Parser s s
element e = satisfies (== e)

parseList :: Parser s a -> Parser s [a]
parseList p = (:) <$> p <*> parseList p <|> pure []

elements :: (Eq s) => [s] -> Parser s [s]
elements = \case
  [] -> pure []
  e : es -> (:) <$> element e <*> elements es

skipWs :: Parser Char ()
skipWs = many (satisfies isSpace) *> ok

parseAtom :: Parser Char a -> Parser Char a
parseAtom p = skipWs *> p <* skipWs

elementsWithoutWs :: String -> Parser Char String
elementsWithoutWs = parseAtom . elements

{-
Logic context-free grammar:
F = E
E = D|D -> E
D = C|D | C
C = N|C & N
N = !N|V|(E)
V = [A..Z][A..Z|0..9|, 39]*
-}

infixl 8 :&

infixl 7 :|

infixr 6 :->

data Expr
  = Expr :& Expr
  | Expr :| Expr
  | Expr :-> Expr
  | ExprNeg Expr
  | ExprVar String
  deriving (Show)

parseVar :: Parser Char Expr
parseVar = do
  h <- satisfies isAsciiUpper
  t <- many (satisfies (\s -> isAsciiUpper s || isDigit s || ord s == 39))
  return $ ExprVar (h : t)

createChainParser :: String -> (a -> a -> a) -> Parser Char a -> Parser Char a
createChainParser delim contstr innerParser =
  innerParser >>= parseChain'
  where
    parseChain' acc = (elementsWithoutWs delim *> (contstr acc <$> innerParser) >>= parseChain') <|> pure acc

parseNeg :: Parser Char Expr
parseNeg = (element '!' *> (ExprNeg <$> parseNeg)) <|> parseVar <|> (elementsWithoutWs "(" *> parseExpr <* elementsWithoutWs ")")

parseConj :: Parser Char Expr
parseConj = createChainParser "&" (:&) parseNeg

parseDisj :: Parser Char Expr
parseDisj = createChainParser "|" (:|) parseConj

parseExpr :: Parser Char Expr
parseExpr = parseDisj >>= parseExpr'
  where
    parseExpr' acc = (elementsWithoutWs "->" *> ((acc :->) <$> parseExpr)) <|> pure acc

parseFile :: Parser Char Expr
parseFile = parseExpr <* eof
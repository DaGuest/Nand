module CompilationEngine where

import Control.Applicative
import JackTokenizer

newtype Parser a = P ([Token] -> [(a, [Token])])

parse :: Parser a -> [Token] -> [(a, [Token])]
parse (P p) inp = p inp

item :: Parser Token
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

-- Checks if given function p satisfies on the next item
sat :: (Token -> Bool) -> Parser Token
sat p = do
  x <- item
  if p x then return x else empty

-- EXPRESSIONS --

--  TERM
term :: Parser [String]
term = do
  termExpr <|> termUnaryOp <|> subroutineCall <|> termList <|> termSingle

termExpr :: Parser [String]
termExpr = do
  e <- exprHookOrBrack ("(", ")")
  return $ wrapXML "term" e

termList :: Parser [String]
termList = do
  v <- getWrappedToken <$> sat isVarNameToken
  e <- exprHookOrBrack ("[", "]")
  return $ wrapXML "term" (v : e)

termSingle :: Parser [String]
termSingle = do
  t <- getWrappedToken <$> sat isTermToken
  return $ wrapXML "term" [t]

termUnaryOp :: Parser [String]
termUnaryOp = do
  u <- getWrappedToken <$> sat isUnaryOpToken
  t <- term
  return $ wrapXML "term" (u : t)

--  EXPRESSION

expr :: Parser [String]
expr = do
  x <- term
  y <- many exprOpTerm
  return $ wrapXML "expression" $ concat (x : y)

exprHookOrBrack :: (String, String) -> Parser [String]
exprHookOrBrack t = do
  pl <- getWrappedToken <$> sat (isGivenSymbol $ fst t)
  e <- expr
  pr <- getWrappedToken <$> sat (isGivenSymbol $ snd t)
  return $ (pl : e) ++ [pr]

exprOpTerm :: Parser [String]
exprOpTerm = do
  x <- op
  z <- term
  return $ x : z

exprList :: Parser [String]
exprList = do
  e <- expr
  es <- many exprListMult
  return $ wrapXML "expressionList" $ concat (e : es)

exprListMult :: Parser [String]
exprListMult = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  e <- expr
  return (s : e)

--  OP

op :: Parser String
op = do
  getWrappedToken <$> sat isOpToken

-- SUBROUTINECALL --

subroutineCall :: Parser [String]
subroutineCall = do
  s <- subroutineCallByName <|> subSubroutineCall
  return $ wrapXML "subroutineCall" s

subroutineCallByName :: Parser [String]
subroutineCallByName = do
  sn <- getWrappedToken <$> sat isVarNameToken
  pl <- getWrappedToken <$> sat (isGivenSymbol "(")
  el <- exprList
  pr <- getWrappedToken <$> sat (isGivenSymbol ")")
  return $ (sn : pl : el) ++ [pr]

subSubroutineCall :: Parser [String]
subSubroutineCall = do
  n <- getWrappedToken <$> sat isVarNameToken
  p <- getWrappedToken <$> sat (isGivenSymbol ".")
  sr <- subroutineCallByName
  return $ n : p : sr

-- STATEMENTS --

statements :: Parser [String]
statements = do
  letSt <|> ifSt

--  LET STATEMENT

letSt :: Parser [String]
letSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "let")
  x <- getWrappedToken <$> sat isVarNameToken
  t <- many $ exprHookOrBrack ("[", "]")
  y <- getWrappedToken <$> sat (isGivenSymbol "=")
  e <- expr
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ k : x : concat t ++ (y : e) ++ [z]

--  WHILE STATEMENT

whileSt :: Parser [String]
whileSt = do
  return ["todo"]

--  IF STATEMENT

ifSt :: Parser [String]
ifSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "if")
  eh <- exprHookOrBrack ("(", ")")
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  el <- concat <$> many elseSt
  return $ k : eh ++ hl : ss ++ hr : el

elseSt :: Parser [String]
elseSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "else")
  ss <- bracketStatements
  return $ k : ss

--  STATEMENT HELPER FUNCTIONS

bracketStatements :: Parser [String]
bracketStatements = do
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ hl : ss ++ [hr]

eval :: [Token] -> [String]
eval ts = case parse statements ts of
  [(xs, [])] -> xs
  [(xs, _)] -> xs
  [] -> error "Invalid input"

-- GENERAL HELPER FUNCTIONS

wrapXML :: String -> [String] -> [String]
wrapXML s xs = (("<" ++ s ++ ">") : xs) ++ ["</" ++ s ++ ">"]

isTermToken :: Token -> Bool
isTermToken (TokKey t) = t `elem` ["true", "false", "null", "this"]
isTermToken (TokInt _) = True
isTermToken (TokIdent _) = True
isTermToken (TokStr _) = True
isTermToken _ = False

isOpToken :: Token -> Bool
isOpToken (TokSymbol s) = s `elem` ["&lt;", "&gt;", "=", "+", "-", "*", "/", "&amp;", "|"]
isOpToken _ = False

isGivenKeyToken :: String -> Token -> Bool
isGivenKeyToken s (TokKey t) = t == s
isGivenKeyToken _ _ = False

isVarNameToken :: Token -> Bool
isVarNameToken (TokIdent _) = True
isVarNameToken _ = False

isGivenSymbol :: String -> Token -> Bool
isGivenSymbol s (TokSymbol t) = t == s
isGivenSymbol _ _ = False

isUnaryOpToken :: Token -> Bool
isUnaryOpToken (TokSymbol s) = s `elem` ["~", "-"]
isUnaryOpToken _ = False
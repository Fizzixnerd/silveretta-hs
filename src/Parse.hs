{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}

module Parse (Program(Program),
              SExpr(Atom,List,Nil),
              Atom(Symbol,Number),
              agParse,
              testText5)
       where

import Data.Functor.Identity
import Control.Applicative
import Text.Parsec hiding (many, (<|>), spaces, newline)
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
  
{- Grammar:
program ::= { sexpr }* <eof>
sexpr ::= explicit-list | implicit-list | atom
explicit-sexpr ::= explicit-list | atom
explicit-list ::= '(' { sexpr }* ')'
implicit-list ::= block-list | line-list
block-list ::= line-list <indent> { line-list }* <outdent>
line-list ::= atom { explicit-sexpr }+ <newline>
atom ::= symbol | number
number ::= <float>
-}

data Program = Program [SExpr] deriving (Show, Eq)
data SExpr = Atom (Atom) | List [SExpr] | Nil deriving (Show, Eq)
data Atom = Symbol String | Number Double deriving (Show, Eq)

-- | Return a Silveretta Program parse tree of the String s.
--parse :: String -> Either ParseError (Program a)
--parse s = IP.parse program "" s

punctuation :: ParsecT String u Identity Char
punctuation = oneOf "{}[]<>#%:.+-*/^&|~!=,"

silverettaDef :: GenLanguageDef String u Identity
silverettaDef = T.LanguageDef { T.commentStart = ";{",
                                T.commentEnd = ";}",
                                T.commentLine = ";",
                                T.nestedComments = True,
                                T.identStart = punctuation <|> letter,
                                T.identLetter = punctuation <|> alphaNum,
                                T.opStart = T.opStart emptyDef,
                                T.opLetter = T.opLetter emptyDef,
                                T.reservedNames = [],
                                T.reservedOpNames = [],
                                T.caseSensitive = True
                              }

agTokP :: T.GenTokenParser String u Identity
agTokP = T.makeTokenParser silverettaDef

indentSize :: Int
indentSize = 2

wrap :: [String] -> [String]
wrap sl = case sl of
  (x:y:zs) -> let indx = indentation x
                  indy = indentation y
              in
               if indy > indx then
                 ((replicate (indy - indx) '(') ++ x):wrap (y:zs)
               else if indy < indx then
                 ((wrap1 x) ++ (replicate (indx - indy) ')')):wrap (y:zs)
               else
                 (wrap1 x):wrap (y:zs)
  [x] -> if (indentation x) /= 0 then
           [(wrap1 x) ++ replicate (indentation x) ')']
         else
           [wrap1 x]
  [] -> []
  where
    indentation line = (length $ fst (span (==' ') line)) `div` indentSize

wrap1 :: String -> String
wrap1 s = "(" ++ s ++ ")"

spaces :: ParsecT String u Identity ()
spaces = T.whiteSpace agTokP

program :: ParsecT String u Identity Program
program = Program <$> many1 sexpr

list :: ParsecT String u Identity SExpr
list = do
  _ <- char '('
  spaces
  l <- many sexpr
  spaces
  _ <- char ')'
  return $ List $ l

sexpr :: ParsecT String u Identity SExpr
sexpr = (try list <|> atom) <* spaces

atom :: ParsecT String u Identity SExpr
atom = Atom <$> (try symbol <|> number)

symbol :: ParsecT String u Identity Atom
symbol = Symbol <$> T.identifier agTokP

number :: ParsecT String u Identity Atom
number = Number <$> T.float agTokP

agParse :: String -> Either ParseError Program
agParse s = parse program "" $ unlines $ (if (length $ lines s) == 1 then lines . wrap1 else wrap . lines) $ s

testText1 = "(hello)"
testText2 = "hello there"
testText3 = "hello\nthere"
testText4 = "hello\n  there"
testText5 = "let\n  x <- 1\n  y <- 4\n  if (> x y)\n    + x y\n    - y x\nlet\n  foo <- bar\n  foo"

pmain :: IO ()
pmain = do
  print $ agParse testText1
  print $ agParse testText2
  print $ agParse testText3
  print $ agParse testText4
  print $ agParse testText5

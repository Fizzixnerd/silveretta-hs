{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}

module Parse (Program(Program),
              SExpr(Atom,List,Nil),
              Atom(Symbol,Number),
              agParse,
              testText5)
       where

import Data.List
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>), parse, spaces, newline)
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol, float)
import Text.Parsec.Language
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as T
import qualified Text.ParserCombinators.Parsec.IndentParser as IP
  
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
data Atom = Symbol String | Number Integer deriving (Show, Eq)

-- | Return a Silveretta Program parse tree of the String s.
--parse :: String -> Either ParseError (Program a)
--parse s = IP.parse program "" s

punctuation = oneOf "{}[]<>#%:.+-*/^&|~!=,"

silverettaDef = LanguageDef { commentStart = ";{",
                              commentEnd = ";}",
                              commentLine = ";",
                              nestedComments = True,
                              identStart = punctuation <|> letter,
                              identLetter = punctuation <|> alphaNum,
                              opStart = opStart emptyDef,
                              opLetter = opLetter emptyDef,
                              reservedNames = [],
                              reservedOpNames = [],
                              caseSensitive = True
                            }

agTokP :: GenTokenParser String u Identity
agTokP = makeTokenParser silverettaDef

indentSize :: Int
indentSize = 2

{-
tokenizeIndents :: String -> String
tokenizeIndents = unlines . map (\line -> unspan $ convertToIndentTokens $ span (==' ') line) . lines
  where
    unspan (x, y) = x ++ y
    convertToIndentTokens (indents, rest) = (unwords $ replicate ((length indents) `div` indentSize) "#<indent> ", rest)

tokenizeNewlines :: String -> String
tokenizeNewlines = unlines . map (\line -> line ++ " #<newline>") . lines
-}

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
  where
    indentation line = (length $ fst (span (==' ') line)) `div` indentSize

wrap1 :: String -> String
wrap1 s = "(" ++ s ++ ")"

spaces :: T.IndentCharParser st ()
spaces = T.whiteSpace agTokP

program :: T.IndentCharParser st Program
program = Program <$> many1 sexpr

list :: T.IndentCharParser st SExpr
list = do
  char '('
  spaces
  l <- many sexpr
  spaces
  char ')'
  return $ List $ l

sexpr :: T.IndentCharParser st SExpr
sexpr = (try list <|> atom) <* spaces

atom :: T.IndentCharParser st SExpr
atom = Atom <$> (try symbol <|> number)

symbol :: T.IndentCharParser st Atom
symbol = Symbol <$> T.identifier agTokP

number :: T.IndentCharParser st Atom
number = Number <$> T.integer agTokP

agParse :: String -> Either ParseError Program
agParse s = IP.parse program "" $ unlines $ (if (length $ lines s) == 1 then lines . wrap1 else wrap . lines) $ s

testText1 = "(hello)"
testText2 = "hello there"
testText3 = "hello\nthere"
testText4 = "hello\n  there"
testText5 = "let\n  x <- 1\n  y <- 4\n  if (> x y)\n    + x y\n    - y x\nlet\n  foo <- bar\n  foo"

pmain = do
  print $ agParse testText1
  print $ agParse testText2
  print $ agParse testText3
  print $ agParse testText4
  print $ agParse testText5

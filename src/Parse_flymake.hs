{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}

module Parse (Program,
              SExpr,
              Atom,
              parse)
       where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>), parse, spaces)
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

data Program a = Program [SExpr a] deriving Show
data SExpr a = Atom (Atom a) | List [SExpr a] | Nil deriving Show
data Atom a = Symbol String | Number a deriving Show

-- | Return a Silveretta Program parse tree of the String s.
parse :: String -> Either ParseError (Program a)
parse s = IP.parse program "" s

punctuation = oneOf "{}[]#<>%;:.+-*/^&|~!=,"

silverettaDef = LanguageDef { commentStart = "/*",
                              commentEnd = "*/",
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

silverettaTokenParser = makeTokenParser silverettaDef

program :: T.IndentCharParser st (Program a)
program = Program <$> many sexpr

sexpr :: T.IndentCharParser st (SExpr a)
sexpr = try explicitSexpr <|> implicitSexpr

implicitSexpr = implicitList

explicitSexpr :: T.IndentCharParser st (SExpr a)
explicitSexpr = try atom <|> explicitList

atom :: T.IndentCharParser st (SExpr a)
atom = Atom <$> symbol <* spaces

symbol :: T.IndentCharParser st (Atom a)
symbol = Symbol <$> T.identifier silverettaTokenParser

explicitList :: T.IndentCharParser st (SExpr a)
explicitList = List <$> T.parens silverettaTokenParser (many explicitSexpr)

spaces :: T.IndentCharParser st ()
spaces = T.whiteSpace silverettaTokenParser

implicitList :: T.IndentCharParser st (SExpr a)
implicitList = do
  firstLine <- lineList
  rest <- IP.block (many sexpr)
  return $ List $ firstLine : rest

lineList :: T.IndentCharParser st (SExpr a)
lineList = do
  l <- List <$> (many explicitSexpr)
  return l

testText1 = "(hello)"
testText2 = "hello there"
testText3 = "hello\n\tthere"

main = parse testText1

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>))
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

program :: T.IndentCharParser st (Program Double)
program = Program <$> many sexpr

sexpr :: T.IndentCharParser st (SExpr a)
sexpr = try explicitSexpr <|> implicitSexpr

implicitSexpr = implicitList

explicitSexpr :: T.IndentCharParser st (SExpr a)
explicitSexpr = try explicitList <|> atom

atom :: T.IndentCharParser st (SExpr a)
atom = Atom <$> symbol

symbol :: T.IndentCharParser st (Atom a)
symbol = Symbol <$> T.identifier silverettaTokenParser

explicitList :: T.IndentCharParser st (SExpr a)
explicitList = List <$> T.parens silverettaTokenParser (many explicitSexpr)

implicitList :: T.IndentCharParser st (SExpr a)
implicitList = do
  firstLine <- lineList
  rest <- IP.block (many sexpr)
  return $ List $ firstLine : rest

lineList :: T.IndentCharParser st (SExpr a)
lineList = List <$> (many sexpr) <* (char '\n')

testText1 = "(hello)"
testText2 = "hello there"
testText3 = "hello\n\tthere"

main = do
  IP.parseTest program testText1
  IP.parseTest program testText2
  IP.parseTest program testText3

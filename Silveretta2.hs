{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol, float)
import Text.Parsec.Language
--import Text.ParserCombinators.Parsec.Number hiding (number)
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

{-
--program :: IndentParser String u (Program Double)
program = do
  p <- many sexpr <* eof
  return $ Program p

--sexpr :: IndentParser String u (SExpr Double)
sexpr = do
  s <- try list <|> atom
  return s

--explicitSexpr :: IndentParser String u (SExpr Double)
explicitSexpr = do
  s <- try (List <$> explicitList) <|> atom <* spaces
  return s

--explicitList :: IndentParser String u [SExpr Double]
explicitList = do
  char '('
  sexprs <- many explicitSexpr
  char ')' <?> "close parenthesis"
  return $ sexprs

--implicitList :: IndentParser String u [SExpr Double]
implicitList = do
  sexprs <- try blockList <|> lineList
  return sexprs

--blockList :: IndentParser String u [SExpr Double]
blockList = do
  bList <- withBlock (:) (List <$> lineList) (List <$> implicitList)
  return bList

--lineList :: IndentParser String u [SExpr Double]
lineList = do
  lHead <- atom
  lTail <- many explicitSexpr <* spaces
  return $ lHead : lTail

--list :: IndentParser String u (SExpr Double)
list = do
  l <- try explicitList <|> implicitList
  return $ List l

--atom :: IndentParser String u (SExpr Double)
atom = do
  a <- try number <|> symbol
  return $ Atom a

-}

--punctuation :: IndentParser String u Char
punctuation = oneOf "{}[]#<>%;:.+-*/^&|~!=,"

{-
--symbol :: IndentParser String u (Atom Double)
symbol = do
  firstChar <- punctuation <|> letter
  rest <- many (punctuation <|> alphaNum)
  return $ Symbol $ firstChar : rest

number = do
  x <- floating
  return $ Number (x :: Double)

--parseIndented :: String -> Either ParseError (Program Double)
parseIndented input = runIndent "" $ runParserT program () "" input

--main :: IO (Either ParseError (Program Double))
main = do
  stuff <- getContents
  return $ parseIndented stuff

-}
  
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
sexpr = try implicitSexpr <|> explicitSexpr

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
    
testText1 = "(hello there)"
testText2 = "hello there"
testText3 = "hello\n\tthere"

main = do
  IP.parseTest program testText1
  IP.parseTest program testText2
  IP.parseTest program testText3

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>), parse, spaces)
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol, float, identifier, colon)
import Text.Parsec.Language
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as T
import qualified Text.ParserCombinators.Parsec.IndentParser as IP
  
{- Grammar:
list ::= { item }*
item ::= header item-block | single-item
header ::= identifier ':' '\n'
item-block ::= <indent> list <outdent>
single-item ::= identifier
identifier ::= letter { letter } *
-}

lang = LanguageDef {
  commentStart = commentStart emptyDef,
  commentEnd = commentEnd emptyDef,
  commentLine = commentLine emptyDef,
  nestedComments = False,
  identStart = letter,
  identLetter = letter,
  opStart = opStart emptyDef,
  opLetter = opLetter emptyDef,
  reservedNames = reservedNames emptyDef,
  reservedOpNames = reservedOpNames emptyDef,
  caseSensitive = True
  }

tok = makeTokenParser lang

data List = List [Item] deriving Show
data Item = Block { blockHeader :: String, blockBody :: List } | Singlet {singletValue :: String} deriving Show

spaces = T.whiteSpace tok

colon = T.colon tok

identifier = T.identifier tok

list :: T.IndentCharParser st List
list = List <$> many item

item :: T.IndentCharParser st Item
item = try block <|> singlet

block :: T.IndentCharParser st Item
block = do
  h <- header
  b <- body
  return $ Block { blockHeader = h, blockBody = b }

header :: T.IndentCharParser st String
header = do 
  name <- identifier
  colon
  spaces
  return name

body :: T.IndentCharParser st List
body = IP.block list

singlet :: T.IndentCharParser st Item
singlet = do
  s <- identifier
  spaces
  return $ Singlet { singletValue = s }

parse :: String -> Either ParseError List
parse = IP.parse list "" 

t1 = "hello"
t2 = "hello:\n\tthere"
t3 = "hello:\n\tthere\n\tdude"
t4 = "hello:\n\tthere\n\tdude\nman"

main = do
  print $ parse t1
  print $ parse t2
  print $ parse t3
  print $ parse t4

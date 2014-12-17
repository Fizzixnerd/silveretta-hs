import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Text.Parsec hiding (many, (<|>), parse)
import Text.Parsec.String
import Text.Parsec.Token hiding (symbol, float)
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

data List = List [Item]
data Item = Block { header :: String, body :: List } | Singlet {value :: String}

list :: T.IndentCharParser st List
list = List <$> many item

item :: T.IndentCharParser st Item
item = try block <|> singlet

block :: T.IndentCharParser

module TigerLexer where

import TigerAbs
import qualified Data.Text as T

import Text.Parsec
import qualified Text.Parsec.Token as Tok
-- import Text.Parsec.Char
import qualified Text.Parsec.Text as PT
-- import Data.Functor.Identity

lexer :: Tok.TokenParser () 
lexer = Tok.makeTokenParser  Tok.LanguageDef
    {Tok.commentStart = "/*"
    ,Tok.commentEnd = "*/"
    ,Tok.commentLine = []
    ,Tok.nestedComments = True
    ,Tok.identStart = letter
    ,Tok.identLetter = alphaNum <|> char '_'
    ,Tok.opStart = oneOf ":,;=&|<=>+-*/." 
    ,Tok.opLetter = char '='
    ,Tok.reservedNames = ["var","let","end","in","function","type","array","of","if","then","else","while","do","for","to","break","nil","()"]
    ,Tok.reservedOpNames = ["=","&","|","<","<=",">",">=","<>","+","-","*","/","\""]
    ,Tok.caseSensitive = False
}

reservedOp = Tok.reservedOp lexer
reserved = Tok.reserved lexer

toInt :: Integer -> Int
toInt = fromInteger
-- number :: PT.Parser Integer
number = do
    n <- Tok.natural lexer
    return (toInt n)
parens = Tok.parens lexer
commaSep = Tok.commaSep lexer
commaSep1 = Tok.commaSep1 lexer
semiSep = Tok.semiSep lexer
semiSep1 = Tok.semiSep1 lexer
identifier = Tok.identifier lexer
dot = Tok.dot lexer
colon = Tok.colon lexer
brackets = Tok.brackets lexer
braces = Tok.braces lexer
symbol = Tok.symbol lexer
stringLiteral = Tok.stringLiteral lexer
whiteSpace = Tok.whiteSpace lexer

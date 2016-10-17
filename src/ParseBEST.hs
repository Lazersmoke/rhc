module ParseBEST where
import Text.Parsec
import BEST

parensParse :: Parser a -> Parser a
parensParse = between (lexeme (char '(') <?> "Open Paren") (lexeme (char ')') <?> "Close Paren")

eol :: Parser Char
eol = lexeme endOfLine

lexeme :: Parser a -> Parser a
lexeme = (<* many (char ' '))

lexString :: String -> Parser ()
lexString = (pure () <*) . lexeme . string

declaration :: Parser Expression
declaration = choice
  [varIDExpression
  ,dataTypeDeclaration
  ,classDeclaration
  ,instanceDeclaration
  ,try valueDeclaration
  ,typeSignatureDeclaration
  ] <?> "Declaration"

varIDExpression :: Parser Expression
varIDExpression = VariableExpression <$> varID

varID :: Parser VarID
varID = VarID <$> 

typeSynonymDeclaration :: Parser Declaration
typeSynonymDeclaration = 
  TypeSynonymDeclaration 
    <$> (lexString "type" *> varName)
    <*> many varName <* lexString "=" 
    <*> typeExpression
    <?> "Type Synonym Declaration"



module ParseFEST where

import FEST

import Text.Parsec
import Text.Parsec.Indent

type IParser a = IndentParser String () a

iParse :: IParser a -> String -> Either ParseError a
iParse p = runIndent "" . runParserT p () "" 

parensParse :: IParser a -> IParser a
parensParse = between (lexeme (char '(') <?> "Open Paren") (lexeme (char ')') <?> "Close Paren")

eol :: IParser Char
eol = lexeme endOfLine

lexeme :: IParser a -> IParser a
lexeme = (<* many (char ' '))

lexString :: String -> IParser ()
lexString = (pure () <*) . lexeme . string

sourceFile :: IParser [Declaration]
sourceFile = spaces *> endBy declaration spaces

declaration :: IParser Declaration
declaration = choice
  [typeSynonymDeclaration
  ,dataTypeDeclaration
  ,classDeclaration
  ,instanceDeclaration
  ,try valueDeclaration
  ,typeSignatureDeclaration
  ] <?> "Declaration"

typeSynonymDeclaration :: IParser Declaration
typeSynonymDeclaration = 
  TypeSynonymDeclaration 
    <$> (lexString "type" *> varName)
    <*> many varName <* lexString "=" 
    <*> typeExpression
    <?> "Type Synonym Declaration"

dataTypeDeclaration :: IParser Declaration
dataTypeDeclaration = 
  DataTypeDeclaration
    <$> (lexString "data" *> context)
    <*> varName 
    <*> manyTill varName (try $ lexString "where" *> eol)
    <*> (indented *> block (typeSignatureDeclaration <* eol))
    <?> "Data Type Declaration"

classDeclaration :: IParser Declaration
classDeclaration = 
  ClassDeclaration
    <$> (lexString "class" *> context)
    <*> varName
    <*> manyTill varName (try $ lexString "where" *> eol)
    <*> (indented *> block ((try typeSignatureDeclaration <|> valueDeclaration) <* eol))
    <?> "Class Declaration"

instanceDeclaration :: IParser Declaration
instanceDeclaration = 
  InstanceDeclaration
    <$> (lexString "instance" *> context)
    <*> varName 
    <*> manyTill (parensParse typeExpression) (try $ lexString "where" *> eol)
    <*> (indented *> block (valueDeclaration <* eol))
    <?> "Instance Declaration"

typeSignatureDeclaration :: IParser Declaration
typeSignatureDeclaration =
  TypeSignatureDeclaration
    <$> varName <* lexString "::"
    <*> typeSignature
    <?> "Type Signature Declaration"

valueDeclaration :: IParser Declaration
valueDeclaration = ValueDeclaration <$> many1 patternClause

patternClause :: IParser PatternClause
patternClause =
  PatternClause
    <$> varName
    <*> many patternParser <* lexString "="
    <*> expression


expression :: IParser Expression
expression = chainl1 atom (pure ApplicationExpression) <?> "Expression"
  where
    atom = parensParse expression <|> variableExpression <|> literalExpression <|> lambdaExpression

lambdaExpression :: IParser Expression
lambdaExpression = 
  LambdaExpression 
    <$> (lexeme (char '\\') *> patternParser) 
    <*> (lexeme (string "->") *> expression)
    <?> "Lambda"

literalExpression :: IParser Expression
literalExpression = LiteralExpression <$> literal

variableExpression :: IParser Expression
variableExpression = VariableExpression <$> varName

typeAnnotatedExpression :: IParser Expression
typeAnnotatedExpression = 
  TypeAnnotatedExpression
    <$> expression <* lexString "::"
    <*> typeSignature
    <?> "Type Annotated Expression"

typeSignature :: IParser TypeSignature
typeSignature = 
  TypeSignature
    <$> context
    <*> typeExpression
    <?> "Type Signature"

context :: IParser Context
context = Context <$> (try assert <|> try asserts <|> noContext) <?> "Context"
  where
    assert = (:[]) <$> contextualAssertation <* lexString "=>"
    asserts = parensParse (contextualAssertation `sepBy1` lexeme (char ',')) <* lexString "=>"
    noContext = pure []

contextualAssertation :: IParser ContextualAssertation
contextualAssertation = 
  ContextualAssertation 
    <$> varName
    <*> many typeExpression
    <?> "Contextual Assertation"

typeExpression :: IParser TypeExpression
typeExpression = chainl1 atom (pure ApplicationTypeExpression) <?> "Type Expression"
  where
    atom = parensParse typeExpression <|> variableTypeExpression

variableTypeExpression :: IParser TypeExpression
variableTypeExpression = VariableTypeExpression <$> varName

varName :: IParser VarName
varName = lexeme $ (VarName .) . (:) <$> letter <*> many alphaNum

patternParser :: IParser Pattern
patternParser = choice
  [varPattern
  ,litPattern
  ,blackHolePattern
  ,constructedPattern
  ] <?> "Pattern"

varPattern :: IParser Pattern
varPattern = VarPattern <$> varName

litPattern :: IParser Pattern
litPattern = LiteralPattern <$> literal

blackHolePattern :: IParser Pattern
blackHolePattern = lexeme (char '_') *> pure BlackHolePattern <?> "Black Hole Pattern"

constructedPattern :: IParser Pattern
constructedPattern = parensParse
  (ConstructedPattern
    <$> varName
    <*> many patternParser
    <?> "Constructor Pattern"
  )

literal :: IParser Literal
literal = choice
  [stringLiteral
  ,charLiteral
  ,intLiteral
  ,fracLiteral
  ] <?> "Literal"

stringLiteral :: IParser Literal
stringLiteral = lexeme $ StringLiteral <$> between (char '\"') (char '\"') (many (noneOf "\""))

charLiteral :: IParser Literal
charLiteral = lexeme $ CharLiteral <$> between (char '\'') (char '\'') anyChar

intLiteral :: IParser Literal
intLiteral = lexeme $ IntLiteral . read <$> many1 digit

fracLiteral :: IParser Literal
fracLiteral = lexeme $ FracLiteral . read <$> do
  w <- many1 digit
  _ <- char '.'
  f <- many1 digit
  return $ w ++ "." ++ f

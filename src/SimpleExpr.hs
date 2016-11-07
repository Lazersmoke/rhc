{-# LANGUAGE PatternSynonyms #-}
module SimpleExpr where

import Text.Parsec
import Text.Parsec.String
import Data.Char

type SimpleExpr = Expr TypedName
type ParsedExpr = Expr Name

data Expr b
  = Application (Expr b) (Expr b)
  | Lambda b (Expr b)
  | PolyLambda b (Expr b)
  | ForAll b (Expr b)
  | Variable b 
  | Literal LiteralValue (Expr b)
  | LitArrow
  deriving (Show,Eq)

instance Functor Expr where
  fmap f (Application a b) = Application (fmap f a) (fmap f b)
  fmap f (Lambda b e) = Lambda (f b) (fmap f e)
  fmap f (PolyLambda b e) = PolyLambda (f b) (fmap f e)
  fmap f (ForAll b e) = ForAll (f b) (fmap f e)
  fmap f (Variable v) = Variable (f v)
  fmap f (Literal v t) = Literal v (fmap f t)
  fmap _ LitArrow = LitArrow

--newtype TypeLayer = TypeLayer SimpleExpr deriving (Eq,Show)

-- (Arrow $ Type (LitKind)) $ Term with type 

compileIt :: String -> Either ParseError (Expr (Unique Name))
compileIt = (typeIt <$>) . (enumIt <$>) . parseIt
  where
    parseIt = parse parseExpr ""
    enumIt = enumerateExpr
    typeIt = id

lexeme :: Parser a -> Parser a
lexeme = (<*(spaces<?>"Lexical Whitespace"))

parseExpr :: Parser ParsedExpr
parseExpr = chainl1 (lexeme realParse <?> "Expression") parseAppl <?> "Applicable Expression"
  where
    realParse = parseParenExpr <|> parseLambda <|> parseVar

parseParenExpr :: Parser ParsedExpr
parseParenExpr = (lexeme (char '(') <?> "Open Paren") *> parseExpr <* (lexeme (char ')') <?> "Close Paren")

parseVar :: Parser ParsedExpr
parseVar = Variable <$> parseName <?> "Variable"

parseAppl :: Parser (Expr a -> Expr a -> Expr a)
parseAppl = pure Application

parseTrivialTypedName :: Parser TypedName
parseTrivialTypedName = Typed <$> pure kindStar <*> (Unique <$> pure 0 <*> parseName)

parseName :: Parser Name
parseName = lexeme (many1 alphaNum) <?> "Name"

parseLambda :: Parser ParsedExpr
parseLambda = 
  Lambda
  <$> (lexeme (char '\\') *> parseName <* lexeme (string "->"))
  <*> parseExpr
  <?> "Lambda"

-- In hind sight I should probably have used a fold or a StateT Int
enumerateExpr :: Expr Name -> Expr (Unique Name)
enumerateExpr = snd . go 0
  where
    go :: Int -> Expr Name -> (Int, Expr (Unique Name))
    go n (Application a b) = (n'',Application a' b')
      where
        (n',a') = go n a
        (n'',b') = go n' b
    go n (Lambda b e) = let (n',e') = go (succ n) e in (n', Lambda (Unique n b) e')
    go n (PolyLambda b e) = let (n',e') = go (succ n) e in (n', PolyLambda (Unique n b) e')
    go n (ForAll b e) = let (n',e') = go (succ n) e in (n', ForAll (Unique n b) e')
    go n (Variable v) = (succ n, Variable (Unique n v))
    go n (Literal v t) = let (n',t') = go n t in (n',Literal v t')
    go n LitArrow = (n,LitArrow)

pattern Function from to <- Application (Application LitArrow from) to

function :: SimpleExpr -> SimpleExpr -> SimpleExpr
function from to = Application (Application LitArrow from) to

-- \a::* -> \b::* -> \x::a::* -> \y::b::* -> x::a::*
-- :: ∀a::* (∀b::* (b::* -> a::*))
-- :: *
--
-- \t::* -> \a::t -> a::t
-- :: ∀t::* (t::* -> t::*)
-- :: *
--
-- \a::Int::* -> \b::Int::* -> a::Int::*
-- :: Int::* -> Int::* -> Int::*
-- :: *

cleanExpr :: SimpleExpr -> String
cleanExpr = printExpr cleanName

fancyExpr :: SimpleExpr -> String
fancyExpr = printExpr showTyped
  where
    showTyped (Typed t (Unique u s)) = s ++ "[" ++ show u ++ "] :: " ++ fancyExpr t

plainExpr :: ParsedExpr -> String
plainExpr = printExpr id

uniqueExpr :: Expr (Unique Name) -> String
uniqueExpr = printExpr showUnique
  where
    showUnique (Unique u s) = s ++ "[" ++ show u ++ "]"

printExpr :: (a -> String) -> Expr a -> String
printExpr f (Function from to) = printExpr f from ++ " -> " ++ printExpr f to
printExpr f LitArrow = "{->}"
printExpr f (Application a b) = printExpr f a ++ " " ++ printExpr f b
printExpr f (Lambda bind e) = "(λ" ++ f bind ++ "." ++ printExpr f e ++ ")"
printExpr f (PolyLambda bind e) = "(POLYλ" ++ f bind ++ "." ++ printExpr f e ++ ")"
printExpr f (ForAll bind e) = "(∀" ++ f bind ++ "." ++ printExpr f e ++ ")"
printExpr f (Variable bind) = f bind
printExpr f (Literal l _) = show l

type Name = {-Source Name-}String
data Unique a = Unique {-Unique-}Int a deriving Show
data Typed a = Typed {-Type-}SimpleExpr a deriving (Show,Eq)

type TypedName = Typed (Unique Name)

typedName :: String -> Int -> SimpleExpr -> TypedName
typedName s u t = Typed t (Unique u s)

instance Eq (Unique a) where
  (==) (Unique ua _) (Unique ub _) = ua == ub

cleanName :: TypedName -> String
cleanName (Typed _ (Unique _ name)) = name

data LiteralValue = IntValue Int | StringValue String deriving (Eq)
instance Show LiteralValue where
  show (IntValue i) = show i
  show (StringValue s) = show s

kindStar :: SimpleExpr
kindStar = Literal (StringValue "*") sort

sort :: SimpleExpr
sort = Literal (StringValue "[%]") sort

idEx :: SimpleExpr
idEx = Lambda x (Variable x)
  where
    x = typedName "x" 0 intType
    intType = (Literal (StringValue "Int") kindStar)

k :: SimpleExpr
k = Lambda x (Lambda underScoreDiscard (Variable x))
  where
    x = typedName "x" 0 intType
    underScoreDiscard = typedName "_" 1 intType
    intType = (Literal (StringValue "Int") kindStar)

--
-- bigLambda = ^a::* -> \x::a -> x::a
-- bigLambda :: forall a. a -> a
-- bigLambda :: Lambda (TypedName "X" 2 kindStar) (Application (Application LitArrow x) x)
--
bigLambda :: SimpleExpr
bigLambda = PolyLambda bigX (Lambda x (Variable x))
  where
    bigX = typedName "X" 0 kindStar
    x = typedName "x" 1 (Variable bigX)

errorTest :: SimpleExpr
errorTest = Application (Lambda x (Variable x)) (Literal (StringValue "meow") (Literal (StringValue "NotInt") kindStar))
  where
    intType = Literal (StringValue "Int") kindStar
    x = typedName "x" 0 intType

-- \x -> x
-- :: forall a. a -> a
-- \@X::* -> \x::X -> x::X
-- :: forall x. x -> x
--
-- (*::sort -> (t::* -> t::*)::*) :: sort -> *
--
-- id :: *::sort -> (t::* -> t::*)::*
-- id = \t::* -> (\x::t -> x::t)
--
-- monoId :: (Int::*) -> (Int::*)
-- monoId = \x::Int -> x::Int
--
-- * :: sort
-- ((Int::*) -> (Int::*)) :: *
-- timesTwo :: Int -> Int
-- timesTwo = \x::Int -> ((times :: Int -> (Int -> Int)) (x::Int) (2::Int))

-- KINDaequal! Haha! get it? KIND? lol
kindaEqual :: SimpleExpr -> SimpleExpr -> Bool
kindaEqual (Literal (StringValue "*") _) (Literal (StringValue "*") _) = True
kindaEqual a b = a == b

data TypeError = MismatchedTypes SimpleExpr SimpleExpr | NotAFunction SimpleExpr SimpleExpr deriving Eq
instance Show TypeError where
  show (MismatchedTypes a b) = "Type {" ++ show a ++ "} did not match {" ++ show b ++ "}"
  show (NotAFunction a b) = "Type {" ++ show a ++ "} is not a function, but was applied to {" ++ show b ++ "}"

getType :: SimpleExpr -> Either TypeError SimpleExpr
getType (Variable (Typed typ _)) = Right typ
--getType (Function funcIn funcOut) = case getType funcIn of
  --Left e -> Left e
  --Right a -> Right $ function funcIn funcOut
getType (Application appA appB) = case getType appA of
  Right (Function funcIn funcOut) -> case getType appB of
    Right appBType -> if kindaEqual funcIn appBType then Right funcOut else Left $ MismatchedTypes funcIn appBType
    Left e -> Left e
  Right a -> Left $ NotAFunction appA appB
  Left e -> Left e
getType (Lambda (Typed typ _) e) = case getType e of
  Right t -> Right $ function typ t
  Left e -> Left e
getType (PolyLambda tyName e) = getType e >>= (Right . ForAll tyName) 
getType (ForAll _ e) = getType e
getType (Literal _ t) = Right t
getType LitArrow = Right $ function kindStar (function kindStar kindStar)

getTypes :: SimpleExpr -> [SimpleExpr]
getTypes e = e : case getType e of
  Right (Literal (StringValue "[%]") _) -> [sort]
  Right typ -> getTypes typ
  Left _ -> []

printTypes :: SimpleExpr -> IO ()
printTypes = mapM_ print . getTypes

substitute :: TypedName -> TypedName -> SimpleExpr -> SimpleExpr
substitute f r (Application a b) = Application (substitute f r a) (substitute f r b)
substitute f r (Lambda b@(Typed t (Unique u s)) e) = 
  Lambda 
    (if b == f then r else typedName s u (substitute f r t)) 
    (substitute f r e)
substitute f r (Variable v@(Typed t (Unique u s))) = 
  if v == f 
    then Variable r 
    else Variable (typedName s u (substitute f r t))
substitute _ _ l@(Literal _ _) = l
substitute _ _ LitArrow = LitArrow

betaReduce :: SimpleExpr -> SimpleExpr
betaReduce d@(Application (Lambda b e) (Variable beta)) = case getType d of
  Left e -> d
  Right _ -> substitute b beta e
betaReduce x = x

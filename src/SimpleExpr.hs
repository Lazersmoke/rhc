{-# LANGUAGE PatternSynonyms #-}
module SimpleExpr where

import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.Char
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State.Lazy



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

instance Foldable Expr where
  foldMap f (Application a b) = foldMap f a <> foldMap f b
  foldMap f (Lambda b e) = f b <> foldMap f e
  foldMap f (PolyLambda b e) = f b <> foldMap f e
  foldMap f (ForAll b e) = f b <> foldMap f e
  foldMap f (Variable v) = f v
  foldMap f (Literal _ t) = foldMap f t
  foldMap _ LitArrow = mempty

compileShow :: String -> IO ()
compileShow = putStrLn . uniqueExpr . (\(Right a) -> a) . compile

compile :: String -> Either ParseError (Expr (Unique Name))
compile = (lexScopeIt . typeIt <$>) . parseIt
  where
    parseIt = parse parseExpr ""
    enumIt = enumerateExpr
    lexScopeIt = lexicallyScopeExpr 
    typeIt = id

lexeme :: Parser a -> Parser a
lexeme = (<*(spaces<?>"Lexical Whitespace"))

parseExpr :: Parser ParsedExpr
parseExpr = chainl1 (lexeme realParse <?> "Expression") parseAppl <?> "Applicable Expression"
  where
    realParse = parseParenExpr <|> parseLambda <|> (Variable <$> parseName)

parseParenExpr :: Parser ParsedExpr
parseParenExpr = (lexeme (char '(') <?> "Open Paren") *> parseExpr <* (lexeme (char ')') <?> "Close Paren")

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
-- Later: Compare this original attempt to the one using a state monad below
-- even Later: Compare this to traverse :P
--enumerateExpr :: Expr Name -> Expr (Unique Name)
--enumerateExpr = snd . go 0
  --where
    --go :: Int -> Expr Name -> (Int, Expr (Unique Name))
    --go n (Application a b) = (n'',Application a' b')
      --where
        --(n',a') = go n a
        --(n'',b') = go n' b
    --go n (Lambda b e) = let (n',e') = go (succ n) e in (n', Lambda (Unique n b) e')
    --go n (PolyLambda b e) = let (n',e') = go (succ n) e in (n', PolyLambda (Unique n b) e')
    --go n (ForAll b e) = let (n',e') = go (succ n) e in (n', ForAll (Unique n b) e')
    --go n (Variable v) = (succ n, Variable (Unique n v))
    --go n (Literal v t) = let (n',t') = go n t in (n',Literal v t')
    --go n LitArrow = (n,LitArrow)

instance Traversable Expr where
--traverse :: Applicative p => (a -> p b) -> Expr a -> p (Expr b)
  traverse f expr = go expr
    where
      go (Application a b) = Application <$> go a <*> go b
      go (Lambda b e) = Lambda <$> f b <*> go e
      go (PolyLambda b e) = PolyLambda <$> f b <*> go e
      go (ForAll b e) = ForAll <$> f b <*> go e
      go (Variable v) = Variable <$> f v
      go (Literal v t) = Literal v <$> go t
      go LitArrow = pure LitArrow

inlineExpr :: Eq b => b -> Expr b -> Expr b -> Expr b
inlineExpr n beta (Application a b) = Application (inlineExpr n beta a) (inlineExpr n beta b) 
inlineExpr n beta (Lambda bind e) = Lambda bind (inlineExpr n beta e)
inlineExpr n beta (PolyLambda bind e) = PolyLambda bind (inlineExpr n beta e)
inlineExpr n beta (ForAll bind e) = ForAll bind (inlineExpr n beta e)
inlineExpr n beta (Variable v) = if n == v then beta else Variable v
inlineExpr n beta (Literal v t) = Literal v (inlineExpr n beta t)
inlineExpr _ _ LitArrow = LitArrow

-- Label every binder in increasing order
enumerateExpr :: Expr Name -> Expr (Unique Name)
enumerateExpr expr = evalState (traverse fresh expr) 0
  where
    fresh :: Name -> State Int (Unique Name)
    fresh v = do
      modify (+1) 
      u <- get
      return (Unique u v)
      
-- \x -> (\x -> x) x
--  1      2    2  1
--
-- \a -> (\a -> a)
--  1      2    2

data LexicalError = UndeclaredVariable String deriving (Eq,Show)

lexicallyScopeExpr :: ParsedExpr -> Expr (Unique Name)
lexicallyScopeExpr expr = evalState (go Map.empty expr) 0
  where
    go :: Map Name Int -> Expr Name -> State Int (Expr (Unique Name))
    -- for Applications, do the first, then the second
    go m (Application a b) = Application <$> go m a <*> go m b
    -- for Lambdas, push the new variable in, then pop it out afterward (don't save m')
    go m (Lambda b e) = do
      u <- fresh
      Lambda (Unique u b) <$> (go (Map.insert b u m) e) 
    go m (PolyLambda b e) = do
      u <- fresh
      PolyLambda (Unique u b) <$> (go (Map.insert b u m) e)
    go m (ForAll b e) = do
      u <- fresh
      ForAll (Unique u b) <$> (go (Map.insert b u m) e)
    -- For variables, check if the Variable is in scope, and use the created Id if it is. Otherwise, error out immediatley
    go m (Variable v) = do
      case Map.lookup v m of
        Just a -> return $ Variable (Unique a v)
        Nothing -> error $ show $ UndeclaredVariable (show v)
    -- Literal Values are not affected by scope
    go m (Literal v t) = Literal v <$> go m t
    -- Literal Arrows are not affected by scope
    go m LitArrow = pure LitArrow

    fresh :: State Int Int
    fresh = const <$> get <*> modify (+1)

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

type SimpleExpr = Expr TypedName
type ParsedExpr = Expr Name

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

addType :: Expr (Unique Name) -> Either TypeError SimpleExpr
addType (Application appA appB) = Application <$> addType appA <*> addType appB
addType (Lambda b e) = Lambda <$> 


printTypes :: SimpleExpr -> IO ()
printTypes = mapM_ print . getTypes

--substitute :: TypedName -> TypedName -> SimpleExpr -> SimpleExpr
--substitute f r (Application a b) = Application (substitute f r a) (substitute f r b)
--substitute f r (Lambda b@(Typed t (Unique u s)) e) = 
  --Lambda 
    --(if b == f then r else typedName s u (substitute f r t)) 
   -- (substitute f r e)
--substitute f r (Variable v@(Typed t (Unique u s))) = 
 -- if v == f 
  --  then Variable r 
   -- else Variable (typedName s u (substitute f r t))
--substitute _ _ l@(Literal _ _) = l
--substitute _ _ LitArrow = LitArrow

betaReduce :: Eq b => Expr b -> Expr b
betaReduce (Application (Lambda b e) beta) = inlineExpr b beta e
betaReduce x = x




{-# LANGUAGE PatternSynonyms #-}
module SimpleExpr where

import Text.Parsec
import Text.Parsec.String
import Data.Char

data SimpleExpr
  = Application SimpleExpr SimpleExpr
  | Lambda TypedName SimpleExpr
  | PolyLambda TypedName SimpleExpr
  | ForAll TypedName SimpleExpr
  | Variable TypedName 
  | Literal LiteralValue SimpleExpr
  | LitArrow
  deriving Eq

--newtype TypeLayer = TypeLayer SimpleExpr deriving (Eq,Show)

-- (Arrow $ Type (LitKind)) $ Term with type 

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

instance Show SimpleExpr where
  show = fancyExpr

showExpr :: SimpleExpr -> String
--showExpr (Function a b) = "(" ++ showExpr a ++ " -> " ++ showExpr b ++ ")"
showExpr (Application a b) = "(" ++ showExpr a ++ "$" ++ showExpr b ++ ")"
showExpr (Lambda bind e) = "(λ" ++ show bind ++ "." ++ showExpr e ++ ")"
showExpr (Variable bind) = show bind
showExpr (Literal i typ) = "(" ++ show i ++ " :: " ++ show typ ++ ")"
showExpr LitArrow = "{->}"

cleanExpr :: SimpleExpr -> String
cleanExpr (Function from to) = fancyExpr from ++ " -> " ++ fancyExpr to
cleanExpr (Application a b) = fancyExpr a ++ " " ++ fancyExpr b
cleanExpr (Lambda bind e) = "(λ" ++ show bind ++ "." ++ fancyExpr e ++ ")"
cleanExpr (Variable bind) = show bind
cleanExpr (Literal l _) = show l
cleanExpr LitArrow = "{->}"

fancyExpr :: SimpleExpr -> String
fancyExpr (Function from to) = fancyExpr from ++ " -> " ++ fancyExpr to
fancyExpr (Application a b) = fancyExpr a ++ " " ++ fancyExpr b
fancyExpr (Lambda bind e) = "(λ" ++ show bind ++ "." ++ fancyExpr e ++ ")"
fancyExpr (PolyLambda bind e) = "(POLYλ" ++ show bind ++ "." ++ fancyExpr e ++ ")"
fancyExpr (ForAll bind e) = "(∀" ++ show bind ++ "." ++ fancyExpr e ++ ")"
fancyExpr (Variable bind) = show bind
fancyExpr (Literal l _) = show l
fancyExpr LitArrow = "{->}"

data TypedName = TypedName {-Source Name-}String {-Unique-}Int {-Type-}SimpleExpr
instance Eq TypedName where
  (==) (TypedName _ ua _) (TypedName _ ub _) = ua == ub

instance Show TypedName where
  show = cleanName
  --show (TypedName name uniq typ) = "(" ++ show uniq ++ "[" ++ name ++ "]" ++ " :: " ++ show typ ++ ")"

cleanName :: TypedName -> String
cleanName (TypedName name _ _) = name

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
    x = TypedName "x" 0 intType
    intType = (Literal (StringValue "Int") kindStar)

k :: SimpleExpr
k = Lambda x (Lambda underScoreDiscard (Variable x))
  where
    x = TypedName "x" 0 intType
    underScoreDiscard = TypedName "_" 1 intType
    intType = (Literal (StringValue "Int") kindStar)

--
-- bigLambda = ^a::* -> \x::a -> x::a
-- bigLambda :: forall a. a -> a
-- bigLambda :: Lambda (TypedName "X" 2 kindStar) (Application (Application LitArrow x) x)
--
bigLambda :: SimpleExpr
bigLambda = PolyLambda bigX (Lambda x (Variable x))
  where
    bigX = TypedName "X" 0 kindStar
    x = TypedName "x" 1 (Variable bigX)

errorTest :: SimpleExpr
errorTest = Application (Lambda x (Variable x)) (Literal (StringValue "meow") (Literal (StringValue "NotInt") kindStar))
  where
    intType = Literal (StringValue "Int") kindStar
    x = TypedName "x" 0 intType

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
getType (Variable (TypedName _ _ typ)) = Right typ
--getType (Function funcIn funcOut) = case getType funcIn of
  --Left e -> Left e
  --Right a -> Right $ function funcIn funcOut
getType (Application appA appB) = case getType appA of
  Right (Function funcIn funcOut) -> case getType appB of
    Right appBType -> if kindaEqual funcIn appBType then Right funcOut else Left $ MismatchedTypes funcIn appBType
    Left e -> Left e
  Right a -> Left $ NotAFunction appA appB
  Left e -> Left e
getType (Lambda (TypedName _ _ typ) e) = case getType e of
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

substitute :: TypedName -> TypedName -> SimpleExpr -> SimpleExpr
substitute f r (Application a b) = Application (substitute f r a) (substitute f r b)
substitute f r (Lambda b@(TypedName s u t) e) = 
  Lambda 
    (if b == f then r else TypedName s u (substitute f r t)) 
    (substitute f r e)
substitute f r (Variable v@(TypedName s u t)) = 
  if v == f 
    then Variable r 
    else Variable (TypedName s u (substitute f r t))
substitute _ _ l@(Literal _ _) = l
substitute _ _ LitArrow = LitArrow

betaReduce :: SimpleExpr -> SimpleExpr
betaReduce d@(Application (Lambda b e) (Variable beta)) = case getType d of
  Left e -> d
  Right _ -> substitute b beta e
betaReduce x = x

{-# LANGUAGE PatternSynonyms #-}
module SimpleExpr where

import Text.Parsec
import Text.Parsec.String
import Data.Char

data SimpleExpr
  = Application SimpleExpr SimpleExpr
  | Lambda TypedName SimpleExpr
  | Variable TypedName 
  | Literal LiteralValue SimpleExpr
  | LitArrow
  deriving Eq

newtype TypeLayer = TypeLayer SimpleExpr deriving (Eq,Show)

-- (Arrow $ Type (LitKind)) $ Term with type 

pattern Function from to <- Application (Application LitArrow from) to
--pattern Arrow <- Application (Application _ LitKind) (Application (Application _ LitKind) LitKind)
-- (->) :: (->) (((->) *) *) *
-- function (function kind kind) kind
-- recArrow :: SimpleExpr -> SimpleExpr
-- recArrow ty = Application (Application (recArrow ty) (Application (Application (recArrow ty) ty) ty)) ty

function :: SimpleExpr -> SimpleExpr -> SimpleExpr
function from to = Application (Application LitArrow from) to

-- (->) :: * -> * -> *
-- (->) :: (((->) *) (((->) *) *))
-- (->) :: (from $ (from $ *))
--arrow :: SimpleExpr
--arrow = arrowOf LitKind
--arrowOf :: SimpleExpr -> SimpleExpr
--arrowOf e = Application fromKind (Application fromKind e)
  --where
    --fromKind = Application arrow e

instance Show SimpleExpr where
  show = fancyExpr

showExpr :: SimpleExpr -> String
--showExpr (Function a b) = "(" ++ showExpr a ++ " -> " ++ showExpr b ++ ")"
showExpr (Application a b) = "(" ++ showExpr a ++ "$" ++ showExpr b ++ ")"
showExpr (Lambda bind e) = "(λ" ++ show bind ++ "." ++ showExpr e ++ ")"
showExpr (Variable bind) = show bind
showExpr (Literal i typ) = "(" ++ show i ++ " :: " ++ show typ ++ ")"
showExpr LitArrow = "{->}"

fancyExpr :: SimpleExpr -> String
fancyExpr (Function from to) = fancyExpr from ++ " -> " ++ fancyExpr to
fancyExpr (Application a b) = fancyExpr a ++ " " ++ fancyExpr b
fancyExpr (Lambda bind e) = "(λ" ++ show bind ++ "." ++ fancyExpr e ++ ")"
fancyExpr (Variable bind) = show bind
fancyExpr (Literal (StringValue s) _) = show s
fancyExpr (Literal (IntValue i) _) = show i
fancyExpr LitArrow = "{->}"

data TypedName = TypedName {-Source Name-}String {-Unique-}Int {-Type-}SimpleExpr
instance Eq TypedName where
  (==) (TypedName _ ua _) (TypedName _ ub _) = ua == ub

instance Show TypedName where
  show (TypedName name uniq typ) = "(" ++ show uniq ++ "[" ++ name ++ "]" ++ " :: " ++ show typ ++ ")"

data LiteralValue = IntValue Int | StringValue String deriving (Eq)
instance Show LiteralValue where
  show (IntValue i) = "LIT{" ++ show i ++ "}"
  show (StringValue s) = "LIT{" ++ show s ++ "}"

kindStar :: SimpleExpr
kindStar = Literal (StringValue "*") kindStar

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

bigLambda :: SimpleExpr
bigLambda = Lambda bigX (Lambda x (Variable x))
  where
    bigX = TypedName "X" 0 kindStar
    x = TypedName "x" 1 (Variable bigX)

type TypeError = String
getType :: SimpleExpr -> Either TypeError SimpleExpr
getType (Variable (TypedName _ _ typ)) = Right typ
getType (Application LitArrow a) = case getType a of
  Left e -> Left e
  Right a -> Right $ function a a
getType (Application l@(Lambda (TypedName _ _ bind) body) beta) = case getType bind of
  Left e -> Left e
  Right a -> case getType beta of
    Left e -> Left e
    Right b -> if a == b then getType body else Left $ "Lambda Argument " ++ show a ++ " doesn't match Beta " ++ show b ++ "!"
getType (Application a b) = case getType a of
  Right (Function b c) -> Right c
  Right a -> Left $ "Invalid Application: " ++ show a ++ " $ " ++ show b
  Left e -> Left e
getType (Lambda (TypedName _ _ typ) e) = case getType e of
  Right t -> Right $ function typ t
  Left e -> Left e
getType (Literal _ t) = Right t
getType LitArrow = undefined

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
{-
data RPS = Rock | Paper | Scissors deriving (Show,Eq)

parseRPS :: Parser RPS
parseRPS = choice [aWord "Rock" *> pure Rock,aWord "Paper" *> pure Paper,aWord "Scissors" *> pure Scissors]
  where
    aWord :: String -> Parser a
    aWord (c:cs) = (char (toLower c) <|> char (toUpper c)) *> aWord cs

beats :: RPS -> RPS -> Maybe Bool
Paper `beats` Rock = Just True
Rock `beats` Scissors = Just True
Scissors `beats` Paper = Just True
a `beats` b = if a == b then Nothing else not <$> (b `beats` a)

doGame :: IO ()
doGame = do
  putStrLn "Rock Paper Scissors! Enter your move below (r,p,s)"
  one <- prompt "Player One's move: "
  two <- prompt "Player Two's move: "
  case one `beats` two of
    Just True -> putStrLn "Player One wins!"
    Just False -> putStrLn "Player Two wins!"
    Nothing -> putStrLn "Tie!" >> doGame
  where
    prompt s = do
      putStr s 
      r <- getLine
      case parse parseRPS "" r of
        Left _ -> prompt s
        Right rps -> return rps
-}

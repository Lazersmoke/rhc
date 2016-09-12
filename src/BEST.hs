module BEST where

import Data.List

data Expression
  = VariableExpression VarID
  | LiteralExpression Literal
  | ApplicationExpression Expression Expression
  -- Case e of 
  | CaseExpression Expression [Case]
  -- LambdaExpression x (VarExp x) = \x -> x
  | LambdaExpression VarID Expression
  -- asdf :: Something
  | TypedExpression Expression CoreType
  -- Polymorphic capital lambda argument
  | TypeExpression CoreType

data CoreType
  = TypeVariableType VarID
  | ApplicationType CoreType CoreType
  | FunctionType CoreType CoreType
  | ForAllType VarID CoreType
  | UnknownType
  -- | LiteralType TypeLiteral
-- GHC.TypeLit BS
-- data TypeLiteral = NumericTypeLiteral Integer | StringTypeLiteral String

instance Show CoreType where
  show (TypeVariableType s) = show s
  -- (Cons 1) Nil
  show (ApplicationType a b) = show a ++ " " ++ tshow b
  show (FunctionType a b@(FunctionType _ _)) = show a ++ " -> " ++ show b
  show (FunctionType a b) = show a ++ " -> " ++ show b
  show (ForAllType v t) = "forall " ++ show v ++ ". " ++ show t
  show UnknownType = "UNINFERENCED TYPE"

tshow :: CoreType -> String
tshow (TypeVariableType s) = show s
tshow a = "(" ++ show a ++ ")"

instance Show Expression where
  show (VariableExpression v) = show v
  show (LiteralExpression l) = show l
  show (CaseExpression e cs) = "case " ++ show e ++ " of\n  " ++ (intercalate "\n  " . map show $ cs)
  show (ApplicationExpression a b) = show a ++ " " ++ vshow b
  show (LambdaExpression a e) = "\\" ++ show a ++ ". " ++ vshow e
  show (TypeExpression t) = "{TYPE " ++ show t ++ "}"
  show (TypedExpression e t) = vshow e ++ " :: " ++ show t

vshow :: Expression -> String
vshow v@(VariableExpression _) = show v
vshow l@(LiteralExpression _) = show l
vshow a = "(" ++ show a ++ ")"

-- Pattern to match, variables bound in said pattern (x,xs for (Cons x xs)), and result expression
data Case = Case Pattern [VarID] Expression

instance Show Case where
  show (Case p _ e) = show p ++ " -> " ++ show e

data Pattern = DataPattern VarID [Pattern] | VariablePattern VarID | LitPattern Literal | DefaultPattern

instance Show Pattern where
  show (DataPattern v ps) = "(" ++ show v ++ " " ++ (unwords . map show $ ps)
  show (VariablePattern v) = show v
  show (LitPattern l) = show l
  show DefaultPattern = "DEFAULT"

data VarID = VarID String Namespace CoreType

instance Show VarID where
  show (VarID a _ t) = "[" ++ a ++ " :: " ++ show t ++ "]"

data Namespace = Type | Value | TypeCons | ValueCons deriving Show

data Literal
  = StringLiteral String
  | CharLiteral Char
  | IntLiteral Integer
  | FracLiteral Double

instance Show Literal where
  show (StringLiteral s) = show s
  show (CharLiteral c) = show c
  show (IntLiteral i) = show i
  show (FracLiteral d) = show d

{-
 - case (Cons 1 Nil) of
 -   (Cons x xs) -> 1
 -   Nil -> 0
 -}

testCase :: Expression
testCase = 
  CaseExpression
    (ApplicationExpression
      (ApplicationExpression
        (VariableExpression (VarID "Cons" ValueCons cons))
        (LiteralExpression (IntLiteral 1))
      )
      (VariableExpression (VarID "Nil" ValueCons list))
    )
    [Case
      (DataPattern 
        (VarID "Cons" ValueCons cons)
        [VariablePattern (VarID "x" Value (TypeVariableType (VarID "a" Type UnknownType)))
        ,VariablePattern (VarID "xs" Value list)
        ]
      )
      [VarID "x" Value (TypeVariableType (VarID "a" Type UnknownType)), VarID "xs" Value list]
      (LiteralExpression (IntLiteral 1))
    ,Case
      (DataPattern (VarID "Nil" ValueCons list) [])
      []
      (LiteralExpression (IntLiteral 0))
    ]
  where
    list = ApplicationType (TypeVariableType (VarID "List" TypeCons UnknownType)) (TypeVariableType (VarID "a" Type UnknownType))
    cons = FunctionType (TypeVariableType (VarID "a" Type UnknownType)) (FunctionType list list)

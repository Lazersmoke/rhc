module BEST where

import Data.List
import Data

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
  | TypeExpression CoreType deriving Show

data CoreType
  = TypeVariableType VarID
  | ApplicationType CoreType CoreType
  | FunctionType CoreType CoreType
  | ForAllType VarID CoreType
  | UnknownType deriving Show
  -- | LiteralType TypeLiteral
-- GHC.TypeLit BS
-- data TypeLiteral = NumericTypeLiteral Integer | StringTypeLiteral String

instance Pretty CoreType where
  pretty (TypeVariableType s) = pretty s
  -- (Cons 1) Nil
  pretty (ApplicationType a b) = pretty a ++ " " ++ tpretty b
  pretty (FunctionType a b@(FunctionType _ _)) = pretty a ++ " -> " ++ pretty b
  pretty (FunctionType a b) = pretty a ++ " -> " ++ pretty b
  pretty (ForAllType v t) = "forall " ++ pretty v ++ ". " ++ pretty t
  pretty UnknownType = "UNINFERENCED TYPE"

tpretty :: CoreType -> String
tpretty (TypeVariableType s) = pretty s
tpretty a = "(" ++ pretty a ++ ")"

instance Pretty Expression where
  pretty (VariableExpression v) = pretty v
  pretty (LiteralExpression l) = pretty l
  pretty (CaseExpression e cs) = "case " ++ pretty e ++ " of\n  " ++ (intercalate "\n  " . map pretty $ cs)
  pretty (ApplicationExpression a b) = pretty a ++ " " ++ vpretty b
  pretty (LambdaExpression a e) = "\\" ++ pretty a ++ ". " ++ vpretty e
  pretty (TypeExpression t) = "{TYPE " ++ pretty t ++ "}"
  pretty (TypedExpression e t) = vpretty e ++ " :: " ++ pretty t

vpretty :: Expression -> String
vpretty v@(VariableExpression _) = pretty v
vpretty l@(LiteralExpression _) = pretty l
vpretty a = "(" ++ pretty a ++ ")"

-- Pattern to match, variables bound in said pattern (x,xs for (Cons x xs)), and result expression
data Case = Case Pattern [VarID] Expression deriving Show

instance Pretty Case where
  pretty (Case p _ e) = pretty p ++ " -> " ++ pretty e

data Pattern = DataPattern VarID [Pattern] | VariablePattern VarID | LitPattern Literal | DefaultPattern deriving Show

instance Pretty Pattern where
  pretty (DataPattern v ps) = "(" ++ pretty v ++ " " ++ (unwords . map pretty $ ps) ++ ")"
  pretty (VariablePattern v) = pretty v
  pretty (LitPattern l) = pretty l
  pretty DefaultPattern = "DEFAULT"

data VarID = VarID String Namespace CoreType deriving Show

instance Pretty VarID where
  --pretty (VarID a _ t) = "[" ++ a ++ " :: " ++ pretty t ++ "]"
  pretty (VarID a _ t) = a 

data Namespace = Type | Value | TypeCons | ValueCons deriving Show

instance Pretty Namespace where
  pretty Type = "type"
  pretty Value = "value"
  pretty TypeCons = "Type"
  pretty ValueCons = "Value"

data Literal
  = StringLiteral String
  | CharLiteral Char
  | IntLiteral Integer
  | FracLiteral Double deriving Show

instance Pretty Literal where
  pretty (StringLiteral s) = show s
  pretty (CharLiteral c) = show c
  pretty (IntLiteral i) = pretty i
  pretty (FracLiteral d) = pretty d

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

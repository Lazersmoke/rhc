module FEST where

import Data.List
import Data

class Crawl c where
  crawl :: (VarName -> VarName) -> c -> c

data Declaration
  -- type Srting = List Char
  = TypeSynonymDeclaration VarName [VarName] TypeExpression
  -- data List a where
  --   Nil :: List a
  --   Cons :: a -> List a -> List a
  | DataTypeDeclaration Context VarName [VarName] [Declaration]
  -- class Functor f where
  --   fmap :: (a -> b) -> f a -> f b
  | ClassDeclaration Context VarName [VarName] [Declaration]
  -- instance Functor (List a) where
  --   fmap f l = map f l
  | InstanceDeclaration Context VarName [TypeExpression] [Declaration]
  | ValueDeclaration [PatternClause]
  | TypeSignatureDeclaration VarName TypeSignature deriving (Show,Eq)

instance Pretty Declaration where
  pretty (TypeSynonymDeclaration v vs ty) = "type " ++ pretty v ++ " " ++ prettyWords vs ++ " = " ++ pretty ty
  pretty (DataTypeDeclaration ctx name params decls) = "data " ++ pretty ctx ++ pretty name ++ " " ++ prettyWords params ++ " where\n" ++ indent (prettyLines decls)
  pretty (ClassDeclaration ctx name params decls) = "class " ++ pretty ctx ++ pretty name ++ " " ++ prettyWords params ++ " where\n" ++ indent (prettyLines decls)
  pretty (InstanceDeclaration ctx name tys decls) = "instance " ++ pretty ctx ++ pretty name ++ " " ++ parens (prettyWords tys) ++ " where\n" ++ indent (prettyLines decls)
  pretty (ValueDeclaration pats) = indent $ prettyLines pats
  pretty (TypeSignatureDeclaration name ty) = pretty name ++ " :: " ++ pretty ty

instance Crawl Declaration where
  crawl f (TypeSynonymDeclaration v vs e) = TypeSynonymDeclaration (f v) (map f vs) (crawl f e)
  crawl f (DataTypeDeclaration ctx v vs ds) = DataTypeDeclaration (crawl f ctx) (f v) (map f vs) (map (crawl f) ds)
  crawl f (ClassDeclaration ctx v vs ds) = ClassDeclaration (crawl f ctx) (f v) (map f vs) (map (crawl f) ds)
  crawl f (InstanceDeclaration ctx v tes ds) = InstanceDeclaration (crawl f ctx) (f v) (map (crawl f) tes) (map (crawl f) ds)
  crawl f (ValueDeclaration pcs) = ValueDeclaration (map (crawl f) pcs)
  crawl f (TypeSignatureDeclaration v ts) = TypeSignatureDeclaration (f v) (crawl f ts)

-- name pat1 pat2 pat3 = exp
data PatternClause = PatternClause VarName [Pattern] Expression deriving (Show,Eq)

instance Pretty PatternClause where
  pretty (PatternClause v pats e) = pretty v ++ " " ++ (unwords . map pretty $ pats) ++ " = " ++ pretty e

instance Crawl PatternClause where
  crawl f (PatternClause v pats e) = PatternClause (f v) (map (crawl f) pats) (crawl f e)
{- fromMaybe :: Maybe a -> a
 - fromMaybe (Just a) = a
 - fromMaybe Nothing = undefined
 -
 - data List :: * -> * where
 -   Nil :: List a
 -   Cons :: a -> List a -> List a
 -}

data Expression
  = LambdaExpression Pattern Expression
  | ApplicationExpression Expression Expression
  | CaseExpression Expression [Case]
  | LiteralExpression Literal
  | VariableExpression VarName
  | TypeAnnotatedExpression Expression TypeSignature deriving (Show,Eq)

parens :: String -> String
parens s = "(" ++ s ++ ")"

parenPretty :: Pretty a => a -> String
parenPretty = parens . pretty

prettyWords :: Pretty a => [a] -> String
prettyWords = unwords . map pretty

prettyLines :: Pretty a => [a] -> String
prettyLines = unlines . map pretty

indent :: String -> String
indent = unlines . map ("  "++) . lines

instance Pretty Expression where
  pretty (LambdaExpression p e) = parens $ "\\" ++ pretty p ++ " -> " ++ pretty e
  pretty (CaseExpression e cs) = "case " ++ pretty e ++ " of\n  " ++ (intercalate "\n  " . map pretty $ cs)
  pretty (ApplicationExpression a b) = pretty a ++ " " ++ vpretty b
  pretty (LiteralExpression l) = pretty l
  pretty (VariableExpression v) = pretty v
  pretty (TypeAnnotatedExpression e ts) = parens $ vpretty e ++ " :: " ++ pretty ts

vpretty :: Expression -> String
vpretty l@(LiteralExpression _) = pretty l
vpretty v@(VariableExpression _) = pretty v
vpretty a = parenPretty a

instance Crawl Expression where
  crawl f (LambdaExpression p e) = LambdaExpression (crawl f p) (crawl f e)
  crawl f (CaseExpression e cs) = CaseExpression (crawl f e) (map (crawl f) cs)
  crawl f (ApplicationExpression a b) = ApplicationExpression (crawl f a) (crawl f b)
  crawl f (LiteralExpression l) = LiteralExpression l
  crawl f (VariableExpression v) = VariableExpression (f v)
  crawl f (TypeAnnotatedExpression e ts) = TypeAnnotatedExpression (crawl f e) (crawl f ts)

data Case = Case Pattern Expression deriving (Show,Eq)

instance Pretty Case where
  pretty (Case p e) = pretty p ++ " -> " ++ pretty e

instance Crawl Case where
  crawl f (Case p e) = Case (crawl f p) (crawl f e)

data Pattern
  -- xs
  = VarPattern VarName
  -- "asdf"
  | LiteralPattern Literal
  -- _
  | BlackHolePattern
  -- ApplicationExpression a b
  -- ConstructedPattern (VarName "ApplicationExpression") [VarPattern (VarName "a"),VarPattern (VarName "b")]
  | ConstructedPattern VarName [Pattern] deriving (Show,Eq)

instance Pretty Pattern where
  pretty (VarPattern v) = pretty v
  pretty (LiteralPattern l) = pretty l
  pretty BlackHolePattern = "_"
  pretty (ConstructedPattern v ps) = parens $ pretty v ++ " " ++ (unwords . map pretty $ ps)

instance Crawl Pattern where
  crawl f (VarPattern v) = VarPattern (f v)
  crawl f (LiteralPattern l) = LiteralPattern l
  crawl f BlackHolePattern = BlackHolePattern
  crawl f (ConstructedPattern v ps) = ConstructedPattern (f v) (map (crawl f) ps)

data Literal
  -- "asdf"
  = StringLiteral String
  -- 'a'
  | CharLiteral Char
  -- 5
  | IntLiteral Integer
  -- 5.53
  | FracLiteral Double deriving (Show,Eq)

instance Pretty Literal where
  pretty (StringLiteral s) = s
  pretty (CharLiteral c) = [c]
  pretty (IntLiteral i) = pretty i
  pretty (FracLiteral f) = pretty f

data TypeExpression
  -- Maybe a
  = ApplicationTypeExpression TypeExpression TypeExpression
  -- a
  | VariableTypeExpression VarName deriving (Show,Eq)

instance Pretty TypeExpression where
  pretty (ApplicationTypeExpression f x) = pretty f ++ " " ++ prettytep x
  pretty (VariableTypeExpression v) = pretty v

prettytep :: TypeExpression -> String
prettytep a = case a of
  (VariableTypeExpression _) -> pretty a
  _ -> parenPretty a

instance Crawl TypeExpression where
  crawl f (ApplicationTypeExpression a b) = ApplicationTypeExpression (crawl f a) (crawl f b)
  crawl f (VariableTypeExpression v) = VariableTypeExpression (f v)

data TypeSignature = TypeSignature Context TypeExpression deriving (Show,Eq)

instance Pretty TypeSignature where
  pretty (TypeSignature ctx typ) = pretty ctx ++ pretty typ

instance Crawl TypeSignature where
  crawl f (TypeSignature ctx typ) = TypeSignature (crawl f ctx) (crawl f typ)

data Context = Context [ContextualAssertation] deriving (Show,Eq)

instance Pretty Context where
  pretty (Context []) = ""
  pretty (Context ctxs) = (++" => ") . parens . intercalate ", " . map pretty $ ctxs

instance Crawl Context where
  crawl f (Context ctas) = Context (map (crawl f) ctas)

data ContextualAssertation = ContextualAssertation VarName [TypeExpression] deriving (Show,Eq)

instance Pretty ContextualAssertation where
  pretty (ContextualAssertation v tys) = pretty v ++ " " ++ (unwords . map pretty $ tys)

instance Crawl ContextualAssertation where
  crawl f (ContextualAssertation v tes) = ContextualAssertation (f v) (map (crawl f) tes)

data VarName = VarName NameSpace String deriving (Show,Eq)

instance Pretty VarName where
  pretty (VarName n s) = s -- ++ " [" ++ pretty n ++ "]"

data NameSpace = TypeCons | ValueCons | Value | Type deriving (Show,Eq)

-- (\x -> x) 5
testCase :: Expression
testCase = ApplicationExpression (LambdaExpression (VarPattern (VarName Value "x")) (VariableExpression (VarName Value "x"))) (LiteralExpression (IntLiteral 5))

-- pretty :: Pretty a => a -> String
{-
testCase' :: Expression
testCase' = TypeAnnotatedExpression (VariableExpression (VarName Value "pretty")) (TypeSignature (Context [ContextualAssertation (VarName TypeCons "Pretty") [VariableTypeExpression (VarName Type "a")]]) (FunctionTypeExpression (VariableTypeExpression (VarName Type "a")) (VariableTypeExpression (VarName TypeCons "String"))))

functorClassTestCase :: Declaration
functorClassTestCase = 
  ClassDeclaration 
    (Context []) 
    (VarName TypeCons "Functor") 
    [VarName Type "f"] 
    [TypeSignatureDeclaration 
      (VarName Value "fmap")
      (TypeSignature
        (Context [])
        (FunctionTypeExpression
          (FunctionTypeExpression
            (VariableTypeExpression (VarName Type "a"))
            (VariableTypeExpression (VarName Type "b"))
          )
          (FunctionTypeExpression
            (ApplicationTypeExpression
              (VariableTypeExpression (VarName Type "f"))
              (VariableTypeExpression (VarName Type "a"))
            )
            (ApplicationTypeExpression
              (VariableTypeExpression (VarName Type "f"))
              (VariableTypeExpression (VarName Type "b"))
            )
          )
        )
      )
    ]
-}

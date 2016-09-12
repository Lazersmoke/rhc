module FESTtoBEST where

import qualified FEST as F
import qualified BEST as B

injectVarNameType :: F.VarName -> B.CoreType -> B.VarID
injectVarNameType (F.VarName F.Type s) t = B.VarID s B.Type t
injectVarNameType (F.VarName F.TypeCons s) t = B.VarID s B.TypeCons t
injectVarNameType (F.VarName F.Value s) t = B.VarID s B.Value t
injectVarNameType (F.VarName F.ValueCons s) t = B.VarID s B.ValueCons t

injectVarName :: F.VarName -> B.VarID
injectVarName v = injectVarNameType v B.UnknownType

runRenamer :: [F.Declaration] -> [F.Declaration] 
runRenamer = undefined

inline :: F.Declaration -> [F.Declaration] -> F.Expression
inline (F.TypeSynonymDeclaration v vs e) = undefined
  where
    doIt (VarName TypeCons s) = 

-- Simplify a single expression
simplify :: F.Expression -> B.Expression
-- Front end lambdas turn in to single case case-expressions in the back end
-- \{n} -> {f}  =>  \a -> case a of {n} -> {f}
simplify (F.LambdaExpression n e) = 
  B.LambdaExpression 
    (B.VarID "a" B.Value B.UnknownType) 
    (B.CaseExpression 
      (B.VariableExpression (B.VarID "a" B.Value B.UnknownType)) 
      -- Build an f-case out of the f-lambda, then simplify it TODO: add default case here for erroring out
      [simplifyCase (F.Case n e)]
    )
-- case e of ...  =>  case e of ...
simplify (F.CaseExpression e cs) = B.CaseExpression (simplify e) (map simplifyCase cs)
-- f x  =>  [f :: a] [x :: b]
simplify (F.ApplicationExpression a b) = B.ApplicationExpression (simplify a) (simplify b)
-- 5  =>  5
simplify (F.LiteralExpression l) = B.LiteralExpression (encodeLiteral l)
-- a  =>  [a :: t]
simplify (F.VariableExpression v) = B.VariableExpression (injectVarName v)
-- Right now we are throwing out the context TODO: Fix that
-- (e :: C t => t)  =>  [e :: t]
simplify (F.TypeAnnotatedExpression e (F.TypeSignature _ t)) = B.TypedExpression (simplify e) (simplifyType t)

simplifyType :: F.TypeExpression -> B.CoreType
simplifyType (F.ApplicationTypeExpression a b) = B.ApplicationType (simplifyType a) (simplifyType b)
simplifyType (F.VariableTypeExpression v) = B.TypeVariableType (injectVarName v)

simplifyCase :: F.Case -> B.Case
-- x -> e binds x
simplifyCase (F.Case p@(F.VarPattern v) e) = B.Case (simplifyPattern p) [injectVarName v] (simplify e)
-- (Cons x xs) -> e binds x, xs
simplifyCase (F.Case p@(F.ConstructedPattern _ _) e) = B.Case (simplifyPattern p) (boundBy p) (simplify e)
  where
    boundBy :: F.Pattern -> [B.VarID]
    boundBy (F.VarPattern v) = [injectVarName v]
    boundBy (F.ConstructedPattern _ ps) = concatMap boundBy ps
    boundBy _ = []
-- other cases don't bind
simplifyCase (F.Case p e) = B.Case (simplifyPattern p) [] (simplify e)

simplifyPattern :: F.Pattern -> B.Pattern
-- x -> e  =>  x -> e
simplifyPattern (F.VarPattern v) = B.VariablePattern (injectVarName v)
-- 5 -> e  =>  5 -> e
simplifyPattern (F.LiteralPattern l) = B.LitPattern (encodeLiteral l)
-- _ -> e  =>  DEFAULTPATTERN -> e  TODO: Figure out why the syntax highlighter can't lex this pattern properly
simplifyPattern F.BlackHolePattern = B.DefaultPattern
-- (Cons x xs) -> e  =>  (Cons x xs) -> e
simplifyPattern (F.ConstructedPattern v ps) = B.DataPattern (injectVarName v) (map simplifyPattern ps)

encodeLiteral :: F.Literal -> B.Literal
encodeLiteral (F.StringLiteral s) = B.StringLiteral s
encodeLiteral (F.CharLiteral c) = B.CharLiteral c
encodeLiteral (F.IntLiteral i) = B.IntLiteral i
encodeLiteral (F.FracLiteral d) = B.FracLiteral d

import Test.HUnit
import qualified FEST as F
import qualified BEST as B
import qualified FESTtoBEST as FB
import qualified ParseFEST as FP

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = test
  ["varName parsing" ~: Right (F.VarName F.Value "asdf") ~=? FP.iParse FP.valueVar "asdf"
  ,"varName parsing invalid" ~: True ~=? either (const True) (const False) (FP.iParse FP.valueVar "'asdf")
  ,"dataDecl parsing invalid" ~: Right testDataDeclGoal ~=? FP.iParse FP.declaration testDataDecl
  ]

testDataDecl :: String
testDataDecl =
  "data List a where\n  Cons :: F a (F (List a) (List a))\n  Nil :: List a\n"

testDataDecl :: String
testDataDecl =
  "class Functor f where\n  Cons :: F a (F (List a) (List a))\n  Nil :: List a\n"

testClassDeclGoal :: F.Declaration
testClassDeclGoal
  F.DataTypeDeclaration 
    (F.Context [])
    (F.VarName F.TypeCons "List") 
    [F.VarName F.Type "a"] 
    [F.TypeSignatureDeclaration
      (F.VarName F.ValueCons "Cons")
      (F.TypeSignature
        (F.Context [])
        (F.ApplicationTypeExpression
          (F.ApplicationTypeExpression
            (F.VariableTypeExpression (F.VarName F.TypeCons "F"))
            (F.VariableTypeExpression (F.VarName F.Type "a"))
          )
          (F.ApplicationTypeExpression
            (F.ApplicationTypeExpression
              (F.VariableTypeExpression (F.VarName F.TypeCons "F"))
              (F.ApplicationTypeExpression
                (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
                (F.VariableTypeExpression (F.VarName F.Type "a"))
              )
            )
            (F.ApplicationTypeExpression
              (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
              (F.VariableTypeExpression (F.VarName F.Type "a"))
            )
          )
        )
      )
    ,F.TypeSignatureDeclaration
      (F.VarName F.ValueCons "Nil")
      (F.TypeSignature
        (F.Context [])
        (F.ApplicationTypeExpression
          (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
          (F.VariableTypeExpression (F.VarName F.Type "a"))
        )
      )
    ]

testDataDeclGoal :: F.Declaration
testDataDeclGoal =
  F.DataTypeDeclaration 
    (F.Context [])
    (F.VarName F.TypeCons "List") 
    [F.VarName F.Type "a"] 
    [F.TypeSignatureDeclaration
      (F.VarName F.ValueCons "Cons")
      (F.TypeSignature
        (F.Context [])
        (F.ApplicationTypeExpression
          (F.ApplicationTypeExpression
            (F.VariableTypeExpression (F.VarName F.TypeCons "F"))
            (F.VariableTypeExpression (F.VarName F.Type "a"))
          )
          (F.ApplicationTypeExpression
            (F.ApplicationTypeExpression
              (F.VariableTypeExpression (F.VarName F.TypeCons "F"))
              (F.ApplicationTypeExpression
                (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
                (F.VariableTypeExpression (F.VarName F.Type "a"))
              )
            )
            (F.ApplicationTypeExpression
              (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
              (F.VariableTypeExpression (F.VarName F.Type "a"))
            )
          )
        )
      )
    ,F.TypeSignatureDeclaration
      (F.VarName F.ValueCons "Nil")
      (F.TypeSignature
        (F.Context [])
        (F.ApplicationTypeExpression
          (F.VariableTypeExpression (F.VarName F.TypeCons "List"))
          (F.VariableTypeExpression (F.VarName F.Type "a"))
        )
      )
    ]


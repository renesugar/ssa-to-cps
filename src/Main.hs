
module Main
    ( main )
where
import Syntax.SSA
import Text.PrettyPrint.Leijen

foo :: Function
foo = Function "foo"
    [ Label "entry"
        [ SAssign (BStatic (Term 1)) (ELit (LInt 3))
        , SAssign (BStatic (Term 2)) (ELit (LInt 5))
        , SAssign (BStatic (Term 3)) (ECall (CPrim "add") [Term 1, Term 2]) ]
    , Label "L0"
        [ SAssign (BStatic (Term 4)) (EPhi (Phi [Term 6] (Term 3)))
        , SAssign (BStatic (Term 5)) (ELit (LInt 1))
        , SAssign (BStatic (Term 6)) (ECall (CPrim "add") [Term 4, Term 5])
        , SAssign (BStatic (Term 7)) (ELit (LInt 10))
        , SAssign (BStatic (Term 8)) (ECall (CPrim "lt") [Term 6, Term 7])
        , SJump (JBranch (Term 8) "L0")
        , SJump (JReturn (Term 8)) ]
    ]

main :: IO ()
main = putDoc (pretty foo <> linebreak)

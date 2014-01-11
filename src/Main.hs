
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
        , SCall (BStatic (Term 3)) (CPrim "add") [Term 1, Term 2]
        , SJump (JReturn (Term 3)) ]
    ]

main :: IO ()
main = putDoc (pretty foo <> linebreak)

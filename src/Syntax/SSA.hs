
module Syntax.SSA
    ( -- * AST
      Prim (..)
    , Lit (..)
    , Term (..)
    , Bind (..)
    , Label (..)
    , Call (..)
    , Jump (..)
    , Phi (..)
    , Expr (..)
    , Stmt (..)
    , Function (..) )
where
import Text.PrettyPrint.Leijen
import Data.Char
import Numeric


data Prim
    = PUnit
    | PUndef

data Lit
    = LInt Integer
    | LString String
    | LPrim Prim

data Term
    = Term Integer                  -- x0 <- ..x1

data Bind
    = BAnon                         -- _  <- ..effect
    | BStatic Term                  -- x0 <- ..x1

data Label
    = Label String [Stmt]           -- L0: ..

data Call
    = CNamed String                 -- call bar ..
    | CPrim String                  -- call $bar ..

data Jump
    = JLabel String
    | JBranch Term String
    | JReturn Term

data Phi
    = Phi [Term] Term

data Expr
    = ELit Lit
    | ETerm Term
    | ECall Call [Term]
    | EPhi Phi

data Stmt
    = SAssign Bind Expr             -- x0 <- ..
    | SJump Jump

data Function
    = Function String [Label]       -- proc foo { .. }


instance Pretty Prim where
    pretty PUnit        = text "()"
    pretty PUndef       = text "_"

instance Pretty Lit where
    pretty (LInt n)     = pretty n
    pretty (LString s)  = pretty s
    pretty (LPrim p)    = pretty p

instance Pretty Term where
    pretty (Term t)     = text "%" <> pretty (showIntAtBase 26 (chr . (+97)) t "")

instance Pretty Bind where
    pretty BAnon        = text "_"
    pretty (BStatic t)  = pretty t

instance Pretty Label where
    pretty (Label s st) = text s <> colon <$> indent 2 (vcat $ map pretty st)

instance Pretty Call where
    pretty (CNamed s)   = text "call" <+> pretty s
    pretty (CPrim s)    = text "call" <+> text "$" <> pretty s

instance Pretty Phi where
    pretty (Phi t s)    = text "phi" <+> list (map pretty t) <+> pretty s

instance Pretty Jump where
    pretty (JLabel n)    = text "goto" <+> pretty n
    pretty (JBranch e s) = text "br" <+> pretty e <+> pretty s
    pretty (JReturn t)   = text "ret" <+> pretty t

instance Pretty Expr where
    pretty (ELit l)     = pretty l
    pretty (ETerm t)    = pretty t
    pretty (ECall c t)  = pretty c <+> tupled (map pretty t)
    pretty (EPhi p)     = pretty p

instance Pretty Stmt where
    pretty (SAssign b e) = pretty b <+> text "<-" <+> pretty e
    pretty (SJump j)     = pretty j

instance Pretty Function where
    pretty (Function s l)
     = text "proc" <+> pretty s <+> lbrace
     <$> indent 2 (vcat $ map pretty l)
     <$> rbrace


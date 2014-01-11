
module Syntax.SSA
    ( -- * AST
      Prim (..)
    , Lit (..)
    , Term (..)
    , Bind (..)
    , Label (..)
    , Call (..)
    , Jump (..)
    , Expr (..)
    , Stmt (..)
    , Function (..) )
where
import Text.PrettyPrint.Leijen


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
    | BPhi Term Term                -- x1 <- (/)(start: ..x0, ..x2)

data Label
    = Label String [Stmt]           -- L0: ..

data Call
    = CNamed String                 -- call bar ..
    | CPrim String                  -- call $bar ..

data Jump
    = JLabel String
    | JBranch Expr String
    | JReturn Term

data Expr
    = ELit Lit
    | ETerm Term

data Stmt
    = SAssign Bind Expr             -- x0 <- ..
    | SCall Bind Call [Term]        -- x0 <- call bar (y1, y2 .. yn)
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
    pretty (Term t)     = text "%" <> pretty t

instance Pretty Bind where
    pretty BAnon        = text "_"
    pretty (BStatic t)  = pretty t
    pretty (BPhi _ t)   = pretty t -- todo, phi doesn't output (start: ..) info

instance Pretty Label where
    pretty (Label s st) = text s <> colon <$> indent 2 (vcat $ map pretty st)

instance Pretty Call where
    pretty (CNamed s)   = text "call" <+> pretty s
    pretty (CPrim s)    = text "call" <+> text "$" <> pretty s

instance Pretty Jump where
    pretty (JLabel n)    = text "goto" <+> pretty n
    pretty (JBranch e s) = text "br" <+> pretty e <+> pretty s
    pretty (JReturn t)   = text "ret" <+> pretty t

instance Pretty Expr where
    pretty (ELit l)     = pretty l
    pretty (ETerm t)    = pretty t

instance Pretty Stmt where
    pretty (SAssign b e) = pretty b <+> text "<-" <+> pretty e
    pretty (SCall b c t) = pretty b <+> text "<-" <+> pretty c <+> tupled (map pretty t)
    pretty (SJump j)     = pretty j

instance Pretty Function where
    pretty (Function s l) = text "proc" <+> pretty s <+> lbrace
      <$> indent 2 (vcat $ map pretty l)
      <$> rbrace


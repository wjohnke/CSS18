module ImpSyntax where

--
-- Abstract syntax for the "Imp" language.
--

-- names (variables) are just strings.
type Name = String

-- a program is a series (list) of statements.
type Prog = [Stmt]

-- a statement is either...
data Stmt =
    Assign Name Exp         -- ...assignment (<name> := <exp>;)
  | If BExp [Stmt] [Stmt]   -- ...if-then-else (if <bexp> { <stmt>* } else { <stmt>* })
  | While BExp [Stmt]       -- ...or a while-loop (while <bexp> { <stmt>*> })
  | Let Name Exp [Stmt]     -- ...let bindings (let <name>=<exp> in { <stmt> *}) 
  deriving Show

-- an integer expression is either...
data Exp =
    Plus Exp Exp            -- ...addition (<exp> + <exp>)
  | Subt Exp Exp            -- ...subtract (<exp> - <exp>)
  | Mult Exp Exp            -- ...multiplication (<exp> * <exp>)
  | Negt Exp                -- ...negation (-<exp>)
  | Var Name                -- ...a variable (<name>)
  | LitInt Int              -- ...or an integer literal (e.g. 3, 0, 42, 1999)
  deriving Show

-- a boolean expression is either...
data BExp =
    IsEq Exp Exp            -- ...test for equality (<exp> == <exp>)
  | IsNEq Exp Exp           -- ...test for inequality (<exp> != <exp>)
  | IsGT Exp Exp            -- ...test for greater-than (<exp> > <exp>)
  | IsLT Exp Exp            -- ...test for less-than (<exp> < <exp>)
  | IsGTE Exp Exp           -- ...test for greater-or-equal (<exp> >= <exp>)
  | IsLTE Exp Exp           -- ...test for less-or-equal (<exp> <= <exp>)
  | And BExp BExp           -- ...boolean and (<bexp> && <bexp>)
  | Or BExp BExp            -- ...boolean or (<bexp> || <bexp>)
  | Not BExp                -- ...boolean negation (!<bexp>)
  | LitBool Bool            -- ... or a boolean literal (true or false)
  deriving Show

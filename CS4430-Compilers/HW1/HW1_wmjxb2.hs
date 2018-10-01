
{-
HW1, CS 4430, Spring 2018.
Due Wednesday, January 31 by 11:59pm.

Directions.

Read these carefully.

There are two problems for HW1, each worth 10 points. 

What to submit: a single file, named HW1_pawprint.hs, where pawprint is
your MU username. The file should contain definitions for every function listed below.
Furthermore, everyone should adhere to the following guidelines to get full credit:

* Your submission must successfully load and typecheck in Haskell Platform to
get any points. For example, executing:
     $ ghci HW1_pawprint.hs
should not produce any errors. I won't attempt to grade assignments that fail to load.

* Name all functions and data types exactly as they appear in the assignment. 

* The code you submit must be your own. Exceptions: you may (of course) use
the code we provide however you like, including examples from the slides and the
book.

* No late submissions---PLEASE START EARLY!

-}

module HW1 where

import ImpSyntax
import ImpParser hiding (expr,stmt)

-- Static checking.
-- Imp programs should not have undefined variables. That is, every variable occurring in an Imp
-- program must be within the body of a "let" statement or else it is undefined.
-- For example:
--    Good: let x := 0 in { x := x + 1; } ; let y := 99 in { y := y + 1; } ;
--    Bad:  let x := 0 in { x := x + 1; } ; x := x + 1 ;
-- In Bad, these occurrences of x           ^    ^  are undefined.

-- In this problem, you will write three "check" functions of the following types:
--    check :: [Stmt] -> [Name] -> Bool
--    checkExp :: Exp -> [Name] -> Bool
--    checkBExp :: BExp -> [Name] -> Bool

-- In a call (check (c:cs) seen), you will inspect c for undefined variables, returning False if you
-- find one and True otherwise. Cases involving variable reference and definitions are the most important.
-- For example, in (check (Let x e cs) seen), you will need to inspect e for undefined variables and then
-- recursively calling check on cs in an expanded "seen list".

-- Problem 1. Fill in the definition of check, checkExp, and checkBExp below.

check :: [Stmt] -> [Name] -> Bool
check [] seen = True
check (c:cs) seen = case c of
					  (Assign x exp)        -> if (checkExp exp seen) then check cs (x:seen) else False
					  (If bexp stmtX stmtY) -> if ( (checkBExp bexp seen) && (check stmtX seen) && (check stmtY seen) ) then check cs seen else False
					  (While bexp x)        -> if ( (checkBExp bexp seen) && (check x seen) ) then check cs seen else False
					  (Let x exp stmtY)     -> if ((checkExp exp seen) && (check stmtY (x:seen)) ) then check cs (seen) else False
---------------------------------------------------------------------------------------------------
checkExp :: Exp -> [Name] -> Bool
checkExp exp seen = case exp of
					  {-
							Continue propagating until base Exp constructor is reached
							Recursively checks left side of Exp equation, if all valid checks
							right side
					  -}
					  (Plus x y) -> if (checkExp x seen) then (checkExp y seen) else False
					  (Subt x y) -> if (checkExp x seen) then (checkExp y seen) else False
					  (Mult x y) -> if (checkExp x seen) then (checkExp y seen) else False
					  (Negt x)   -> if (checkExp x seen) then True else False
					  --Base cases of variable name or literal
					  (LitInt x) -> True
					  (Var x)    -> checkVariable x seen
						where checkVariable :: Name -> [Name] -> Bool
						      checkVariable _ [] = False
							  ;checkVariable x (name:seen) = if (x==name) then True else checkVariable x seen

					  

checkBExp :: BExp -> [Name] -> Bool
checkBExp bexp seen = case bexp of
					    (IsEq x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (IsNEq x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (IsGT x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (IsLT x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (IsGTE x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (IsLTE x y) -> if ( (checkExp x seen) && (checkExp y seen) ) then True else False
					    (And x y) -> if ((checkBExp x seen) && (checkBExp y seen)) then True else False
					    (Or x y) -> if ((checkBExp x seen) && (checkBExp y seen)) then True else False
					    (Not x) -> if ((checkBExp x seen)) then True else False
					    (LitBool x) -> True
						


-- Problem 2. Alter the interp function so that, if there is an undefined variable in the
-- Imp code, that the interpreter is not called. Instead of returning the (Maybe Store) as it currently
-- does, change interp to:
--    1. print out the value of "ans", if there are no undefined variables;
--    2. print out "Undefined Variable" otherwise.

-- The type of interp will change to:
--             interp :: FilePath -> IO ()
-- To print to the terminal, use the following built-in function:
--        putStrLn :: String -> IO ()
--
-- Sample output is as follows:
--    ghci> interp "succeed1.imp"
--    Answer is: 0
--    ghci> interp "fail1.imp"
--    Undefined Variable

-- There are a number of test cases, named either "succeed" or "fail" which should produce the output
-- corresponding to their name.
interp :: FilePath -> IO ()
interp f = do
		       cs <- parseImp f
			   ;if (check cs []) then (putStrLn ("Answer is: " ++ (show(interpret cs) ))) else putStrLn "Undefined Variable"
			   
interpret :: [Stmt] -> Maybe Int
interpret cs =do 
			      ans <- stmts cs sto0
				  ;return (snd (head (ans)) )

type Store = [(Name,Int)]
sto0       = [("ans",0)] -- initial store

replace :: Name -> Int -> Store -> Maybe Store
replace x i []         = Nothing 
replace x i ((y,v):cs) | x == y = return $ (x,i) : cs
                       | x /= y = do
                                     cs' <- replace x i cs
                                     return $ (y,v) : cs'

stmts :: [Stmt] -> Store -> Maybe Store
stmts [] _     = Nothing -- "empty" program
stmts (c:cs) s = do
                    s' <- stmt c s
                    case cs of
                         [] -> return s'
                         _  -> stmts cs s'

stmt :: Stmt -> Store -> Maybe Store
stmt (Assign x e) s    = do
                            i <- expr e s
                            s' <- replace x i s
                            return s'
stmt (If be cs1 cs2) s = do
                            bv <- bexp be s
                            if bv then stmts cs1 s else stmts cs2 s
stmt l@(While be cs) s = do
                            bv <- bexp be s
                            if bv then stmts (cs++[l]) s else return s
stmt (Let x e cs) s    = do
                            i <- expr e s
                            s' <- stmts cs ((x,i):s)
                            pop s'
     where
       pop :: Store -> Maybe Store
       pop []           = Nothing
       pop (cell:cells) = return cells


expr :: Exp -> Store -> Maybe Int
expr (Plus e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 + i2
expr (Subt e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 - i2
expr (Mult e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 * i2
expr (Negt e) s     = do
                         i <- expr e s
                         return $ - i
expr (Var x) s      = lookup x s
expr (LitInt i) s   = return i

bexp :: BExp -> Store -> Maybe Bool
bexp (IsEq e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 == i2
bexp (IsNEq e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 /= i2
bexp (IsGT e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 > i2
bexp (IsLT e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 < i2
bexp (IsGTE e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 >= i2
bexp (IsLTE e1 e2) s = do
                         i1 <- expr e1 s
                         i2 <- expr e2 s
                         return $ i1 <= i2
bexp (And b1 b2) s = do
                         bv1 <- bexp b1 s
                         bv2 <- bexp b2 s
                         return $ bv1 && bv2
bexp (Or b1 b2) s  = do
                         bv1 <- bexp b1 s
                         bv2 <- bexp b2 s
                         return $ bv1 || bv2
bexp (Not b) s      = do
                         tf <- bexp b s
                         return $ not tf
bexp (LitBool bv) s = return bv



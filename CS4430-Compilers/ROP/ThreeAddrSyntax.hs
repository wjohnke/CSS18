module ThreeAddrSyntax where

import Prelude hiding (Word)
import Common
import Control.Monad.Identity
import Control.Monad.State

data ThreeAddrProg = ThreeAddrProg [ThreeAddr]
     
data ThreeAddr = Mov Register Arg
               | Load Register Register   -- (Load rtarg raddr) is rtarg := contents(raddr)
               | Store Register Register  -- (Store rt rs) stores contents(rs) in address rt
               | Add Register Arg Arg
               | Sub Register Arg Arg
               | Mul Register Arg Arg
               | Negate Register Arg
               | Equal Register Arg Arg
               | LogNot Register Arg 
               | GThan Register Arg Arg
               | Jmp Arg         
               | BrZ Register Arg
               | BrNZ Register Arg
               | Call Arg
               | Ret
               | Label Int
               | Read Register 
               | Write Arg
               | Exit
               | Pop Register      -- new
               | Push Arg          -- new
               | PushLabel Int     -- new

instance Show ThreeAddrProg where
   show (ThreeAddrProg ts) = pretty ts
     where
       pretty []         = ""
       pretty (t : rest) = case t of
         Label _ -> show t ++ pretty rest
         _       -> '\t' : show t ++ ";\n" ++ pretty rest

instance Show ThreeAddr where
    show (Mov ra a)      = "mov " ++ show ra ++ " "++show a 
    show (Load r1 r2)    = "load " ++ show r1 ++ " "++show r2
    show (Store r1 r2)   = "store " ++ show r1 ++ " "++show r2
    show (Add ra a1 a2)  = "add " ++ show ra ++" " ++ show a1 ++ " " ++ show a2
    show (Sub ra a1 a2)  = "sub "++ show ra ++ " " ++ show a1 ++" " ++ show a2
    show (Mul ra a1 a2)  = "mul " ++ show ra++" "++show a1++" "++show a2
    show (Negate ra a)   = "neg "++show ra++" "++show a
    show (Equal r a1 a2) = "equal " ++show r++" "++show a1++" "++show a2
    show (LogNot ra a)   = "neg "++show ra++" "++show a
    show (GThan r a1 a2) = "equal " ++show r++" "++show a1++" "++show a2
    show (Jmp a)         = "jmp "++show a
    show (BrZ r a)       = "brz "++show r++" "++show a
    show (BrNZ r a)      = "brnz "++show r++" "++show a
    show (Label i)       = show i ++ ":"
    show (Read ra)       = "read "++show ra
    show (Write a)       = "write "++show a
    show (Call a)        = "call "++show a
    show Ret             = "ret"
    show Exit            = "exit"
    show (Pop ra)        = "pop " ++ show ra
    show (Push ra)       = "push " ++ show ra
    show (PushLabel ra)  = "pushl " ++ show ra

normalize :: [ThreeAddr] -> [(Int, ThreeAddr)]
normalize tac = chglabels seen im
   where Identity ((im,seen),_) = runStateT (norm [] [] tac) 0

norm :: [(Int, ThreeAddr)] ->
        [(Int, Int)]       ->
        [ThreeAddr]        ->
        StateT Int Identity ([(Int, ThreeAddr)], [(Int, Int)])
norm acc lacc []     = return (reverse acc,lacc)
norm acc lacc (c:cs) = case (c,cs) of
  (Label i,c':cs')  -> do
    a <- nextAddr
    norm ((a,c') : acc) ((i,a):lacc) cs'
  (Label i,[])      -> do
    a <- nextAddr
    return (reverse ((a,Exit) : acc),(i,a) : lacc)
  (_,_)             -> do
    a <- nextAddr
    norm ((a,c) : acc) lacc cs
  
nextAddr :: Monad m => StateT Int m Int
nextAddr = do
  addr <- get
  put (addr+1)
  return addr

chglabels :: [(Int,Int)] -> [(Int,ThreeAddr)] -> [(Int,ThreeAddr)]
chglabels seen []         = []
chglabels seen ((a,c):im) = case c of
  PushLabel l       -> (a,PushLabel l') : chglabels seen im
     where Just l' = lookup l seen
  Jmp (Literal l)   -> (a,Jmp (Literal l')) : chglabels seen im
     where Just l' = lookup l seen
  BrZ reg (Literal l) -> (a,BrZ reg (Literal l')) : chglabels seen im
     where Just l' = lookup l seen
  BrNZ reg (Literal l)   -> (a,BrNZ reg (Literal l')) : chglabels seen im
     where Just l' = lookup l seen
  Call (Literal l)       -> (a,Call (Literal l')) : chglabels seen im
     where Just l' = lookup l seen
  _                 -> (a,c) : chglabels seen im         



    

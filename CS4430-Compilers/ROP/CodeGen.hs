module CodeGen where

import Common
import ImpSyntax
import ThreeAddrSyntax
import Control.Monad.Identity
import Control.Monad.State

type M = StateT Label (StateT VirtualReg Identity)

runM :: M a -> a
runM (StateT phi) = v
  where StateT phi' = phi 1
        Identity ((v,_),_) = phi' 0

newlabel :: M Label
newlabel = do
  l <- get
  modify (\ l -> l + 1)
  return l

newreg :: M Register
newreg = lift $ do
  l <- get
  modify (\ l -> l + 1)
  return (Reg $ show l)

compileExp :: Exp -> M ([ThreeAddr],Register)
compileExp (Var x)    = return ([],Reg x)
compileExp (LitInt i) = do
  ri <- newreg
  return ([Mov ri (word i)],ri)
compileExp (Plus e1 e2) = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  rhs     <- newreg
  return (p1 ++ p2 ++ [Add rhs (register r1) (register r2)], rhs)
compileExp (Subt e1 e2) = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  rhs     <- newreg
  return (p1 ++ p2 ++ [Sub rhs (register r1) (register r2)], rhs)
compileExp (Mult e1 e2) = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  rhs     <- newreg
  return (p1 ++ p2 ++ [Mul rhs (register r1) (register r2)], rhs)
compileExp (Negt e) = do
  (p,r) <- compileExp e
  rhs     <- newreg
  return (p ++ [Negate rhs (register r)], rhs)

-- Using C conventions for true and false.
--     false is 0
--     true value is non-zero  
--  
compileBExp :: BExp -> M ([ThreeAddr],Register)

compileBExp (LitBool tf) = do
  r <- newreg
  let encodedtf = case tf of
        True  -> 1
        False -> 0
  return ([Mov r (word encodedtf)],r)

compileBExp (IsEq e1 e2)    = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  cmp     <- newreg
  lab     <- newlabel
  out     <- newlabel
  return $ (p1 ++
            p2 ++
           [Sub cmp (register r1) (register r2), -- compute e1-e2
            BrNZ cmp (word lab),                  -- if NZ, then false, else true
            Mov cmp (word 1),                     -- cmp := True
            Jmp (word out),
            Label lab,
            Mov cmp (word 0),                     -- cmp := False
            Label out], cmp)
compileBExp (IsNEq e1 e2)    = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  cmp     <- newreg
  lab     <- newlabel
  out     <- newlabel
  return $ (p1 ++
            p2 ++
           [Sub cmp (register r1) (register r2), -- compute e1-e2
            BrNZ cmp (word lab),                  -- if NZ, then true, else false
            Mov cmp (word 0),                     -- cmp := False
            Jmp (word out),
            Label lab,
            Mov cmp (word 1),                     -- cmp := True
            Label out], cmp)

compileBExp (IsGT e1 e2) = do
  (p1,r1) <- compileExp e1
  (p2,r2) <- compileExp e2
  ans <- newreg
  return (p1 ++
          p2 ++
         [GThan ans (register r1) (register r2)],
          ans)
compileBExp (IsLT e1 e2)  = compileBExp (IsGT e2 e1)
compileBExp (IsGTE e1 e2) = compileBExp (Or (IsGT e1 e2) (IsEq e1 e2))
compileBExp (IsLTE e1 e2) = compileBExp (Or (IsGT e2 e1) (IsEq e1 e2))

compileBExp (And b1 b2) = do
  (p1,r1) <- compileBExp b1
  (p2,r2) <- compileBExp b2
  ans     <- newreg
  labF    <- newlabel
  exit    <- newlabel
  return ( p1                  ++
          [BrZ r1 (word labF)] ++
           p2                  ++
          [BrZ r2 (word labF)  ,
           Mov ans (word 1)    ,  -- if you reach here, (And b1 b2) is True.
           Jmp (word exit)     ,
           Label labF          ,
           Mov ans (word 0)    ,   -- if you reach here, (And b1 b2) is False.
           Label exit],
           ans)
           
compileBExp (Or b1 b2) = do
  (p1,r1) <- compileBExp b1
  (p2,r2) <- compileBExp b2
  ans     <- newreg
  labT    <- newlabel
  exit    <- newlabel
  return ( p1                   ++
          [BrNZ r1 (word labT)] ++
           p2                   ++
          [BrNZ r2 (word labT)  ,
           Mov ans (word 0)     ,  -- if you reach here, (Or b1 b2) is False.
           Jmp (word exit)      ,
           Label labT           ,
           Mov ans (word 1)     ,  -- if you reach here, (Or b1 b2) is True.
           Label exit],
           ans)

compileBExp (Not b)          = do
  (p,r) <- compileBExp b
  ans   <- newreg
  lab   <- newlabel
  out   <- newlabel
  return  (p ++
          [BrNZ r (word lab), -- branch if r is True
           Mov r (word 1),    -- set r to True
           Jmp (word out),
           Label lab,
           Mov r (word 0),    -- set r to False
           Label out],
           r)
  

compileStmt :: Stmt -> M [ThreeAddr]
compileStmt (Assign x e) = do
  (p,r) <- compileExp e
  return $ p ++ [Mov (Reg x) (register r)]
    
compileStmt (If b s1 s2) = do
  l1    <- newlabel
  l2    <- newlabel
  exit  <- newlabel
  p1    <- compileStmts s1
  p2    <- compileStmts s2
  (p,r) <- compileBExp b
  return $ p ++
          [BrZ r (word l2)] ++
           p1                ++
          [Jmp (word exit),
           Label l2]         ++
           p2                ++
          [Label exit]

compileStmt (While b s)  = do
  test   <- newlabel
  exit   <- newlabel
  (pb,r) <- compileBExp b
  ps     <- compileStmts s
  return $ [Label test] ++
            pb          ++
           [BrZ r (word exit)] ++ -- if b is False, then exit
            ps                 ++ -- otherwise, do s
           [Jmp (word test),      -- re-test b
            Label exit]
            
compileStmt (Let x e s)  = do
  (pe,r) <- compileExp e
  ps     <- compileStmts s
  return $ pe ++
          [Mov (Reg x) (register r)] ++
           ps
           
compileStmts :: [Stmt] -> M [ThreeAddr]
compileStmts []     = return []
compileStmts (c:cs) = do
  p  <- compileStmt c
  ps <- compileStmts cs
  return $ p ++ ps

compileImp :: Prog -> StateT Label (StateT VirtualReg Identity) ThreeAddrProg
compileImp imp = do
  p <- compileStmts imp
  return $ ThreeAddrProg $ Label 0 : p ++ [Exit]

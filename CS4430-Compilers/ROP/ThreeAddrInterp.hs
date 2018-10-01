module ThreeAddrInterp where

import Common
import Control.Monad.Identity
import Control.Monad.State
import ThreeAddrParser
import ThreeAddrSyntax

type M        = StateT RegFile (StateT Mem IO)
type RegFile  = Register -> Int
type Address  = Int
type Mem      = Address -> Int
type InstrMem = [(Address,ThreeAddr)]

(+>) :: Eq a => (a,b) -> (a -> b) -> a -> b
(x,v) +> f = \ a -> if a==x then v else f a

readReg :: Register -> M Int
readReg r = do
  rf <- get
  return (rf r)

setReg :: Register -> Int -> M Next
setReg r v = modify ((r,v) +>) >> return Cont

readLoc :: Address -> M Int
readLoc l = do
  dm <- lift $ get
  return $ dm l  

setLoc :: Address -> Int -> M Next
setLoc l v = lift $ modify ((l,v) +>) >> return Cont

readStk :: Address -> M Int
readStk l = do
  sm <- lift $ get
  return $ sm l  

setStk :: Address -> Int -> M ()
setStk l v = lift $ modify ((l,v) +>)

push :: Int -> M Next
push v = do
  sp <- readReg SP
  setStk (sp+1) v
  setReg SP (sp+1)

pop :: M Int
pop = do
  sp <- readReg SP
  if sp < 0
    then error "can't pop empty stack!"
    else do
           v <- readStk sp
           setReg SP (sp-1)
           return v

eval :: Arg -> M Int
eval (Immediate r) = readReg r
eval (Literal w)   = return w  

data Next = Cont | Halt | Jump Address | FunctionCall Address deriving Show

exec :: ThreeAddr -> M Next
exec instr = do
  case instr of
   Mov reg arg         -> do
     v <- eval arg
     setReg reg v
     return Cont
   Load rtrg radr      -> do
     addr <- readReg radr
     v    <- readLoc addr
     setReg rtrg v
     return Cont
   Store rtrg rsrc     -> do
     trg <- readReg rtrg
     src <- readReg rsrc
     setLoc trg src
     return Cont
   Add reg arg1 arg2   -> do
     v1 <- eval arg1
     v2 <- eval arg2
     setReg reg (v1 + v2)
     return Cont
   Sub reg arg1 arg2   -> do
     v1 <- eval arg1
     v2 <- eval arg2
     setReg reg (v1 - v2)
     return Cont
   Mul reg arg1 arg2   -> do
     v1 <- eval arg1
     v2 <- eval arg2
     setReg reg (v1 * v2)
     return Cont
   Negate reg arg      -> do
     v <- eval arg
     setReg reg (-v)
     return Cont
   Equal reg arg1 arg2 -> do
     v1 <- eval arg1
     v2 <- eval arg2
     setReg reg (if v1==v2 then 1 else 0)
     return Cont
   LogNot reg arg      -> do
     v <- eval arg
     setReg reg (if v==0 then 1 else 0)
     return Cont
   GThan reg arg1 arg2 -> do
     v1 <- eval arg1
     v2 <- eval arg2
     setReg reg (if v1>v2 then 1 else 0)
   Jmp arg             -> do
     v  <- eval arg
     return (Jump v)
   BrZ reg arg         -> do
     v  <- eval arg
     rv <- readReg reg
     if rv==0 then return (Jump v) else return Cont
   BrNZ reg arg        -> do
     v  <- eval arg
     rv <- readReg reg
     if rv==0 then return Cont else return (Jump v) 
   Label i             -> return Cont
   Read reg            -> do
     str <- lift $ lift getLine
     setReg reg (read str)
   Write arg           -> do
     v <- eval arg
     lift $ lift $ putStrLn $ "Output: " ++ show v
     return Cont
   Call arg            -> do
     v  <- eval arg
     return (FunctionCall v)
   Ret                 -> do
     rl <- pop
     return (Jump rl)
   Exit                -> return Halt
   Pop reg             -> do
     v <- pop
     setReg reg v
   Push arg            -> do
     v <- eval arg
     push v
   PushLabel l         -> push l >> return Cont

report pc sp atsp i = do
  lift $ lift $ putStr $ "pc:" ++ show pc ++ "; sp:" ++ show sp ++ "; [sp]:" ++ show atsp ++ "; " ++ show i
  lift $ lift $ getLine
  
repl instrmem pc = case lookup pc instrmem of
  Just i  -> do
      sp   <- readReg SP
      atsp <- readLoc sp
      report pc sp atsp i
      code <- exec i
      case code of
       Cont   -> repl instrmem (pc+1)
       Halt   -> return ()
       Jump a -> repl instrmem a
       FunctionCall a -> do
          push (pc+1)
          repl instrmem a
  Nothing -> error "Tried to read code that wasn't there"

--interp :: FilePath -> IO ()
interp tac_ = do
  p   <- Common.expandFilePath tac_
  tacsrc <- readFile p
  let Just tac = front3addr tacsrc
  let im = normalize tac
  putStrLn "Compiled code:\n"
  putStrLn $ ppInstrMem im
  runM rf0 mem0 (repl im 0)
     where
       rf0  = \ r -> 0
       mem0 = \ a -> 0

runM :: RegFile -> Mem -> M a -> IO (a,RegFile,Mem)
runM rf mem x = do
   ((v,rf'),mem') <- runStateT (runStateT x rf) mem
   return (v,rf',mem')

ppInstrMem [] = ""
ppInstrMem ((l,i):ims) = show l ++ ":\t" ++ show i ++ ";\n" ++ ppInstrMem ims
    
-- how to use getLine and read
foobar :: IO Int
foobar = do
  num <- getLine
  return $ 1 + read num

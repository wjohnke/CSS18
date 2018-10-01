module Common where

import Prelude hiding (Word)
import System.Environment

type Label      = Int
type VirtualReg = Int 
type Word       = Int
data Register   = Reg String | PC | SP | FP | BP deriving Eq

data Arg        = Immediate Register | Literal Word 

instance Show Register where
    show (Reg i) = "R" ++ i
    show SP      = "SP"
    show BP      = "BP"
    show FP      = "FP"

instance Show Arg where
  show (Immediate r) = show r
  show (Literal w)   = '#' : show w

register :: Register -> Arg
register = Immediate

word :: Word -> Arg
word = Literal

(<>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f <> g = \ a -> f a >>= g

expandFilePath :: FilePath -> IO FilePath
expandFilePath ('~':'/':s) = do hd <- getEnv "HOME"
                                return (hd ++ "/" ++ s)
expandFilePath s           = return s

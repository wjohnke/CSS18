module Main where

import BasicBlocks
import CodeGen
import Common
import Control.Monad.Identity
import Control.Monad.State
import Data.Graph
import ImpParser
import ImpSyntax
import System.Process
import ThreeAddrParser
import ThreeAddrSyntax

codegen :: FilePath -> IO ()
codegen imp = do
  impprog <- parseImp imp
  putStrLn $ show $ runM $ compileImp impprog




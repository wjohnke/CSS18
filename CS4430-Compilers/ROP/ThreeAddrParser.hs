module ThreeAddrParser where

import System.IO
import Data.Char

import Common
import ThreeAddrSyntax

data Token = MOV | LOAD | STORE | ADD | SUB | MUL | NEGATE | EQUAL | NOT | GTHAN |
             JMP | BRZ | BRNZ | READ | WRITE | CALL | RET | EXIT |
             REG String | LIT Int | FPtok | SPtok | BPtok | SEMICOL | COLON | ENDOFINPUT |
             POP | PUSH | PUSHL
                                   deriving (Eq,Show)

front3addr :: String -> Maybe [ThreeAddr]
front3addr = lexer <> parse3addr

--
-- The lexer.
--
lexer :: String -> Maybe [Token]
lexer [] = return [ENDOFINPUT]
lexer ('/':'/':cs) = consumeLine cs -- comments
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexAlpha (c:cs)
     | isDigit c = lexNum (c:cs)
     | c==';'    = do
                      rest <- lexer cs
                      return $ SEMICOL : rest
     | c==':'    = do
                      rest <- lexer cs
                      return $ COLON : rest
     | c=='#'    = lexNum cs
     | otherwise = Nothing

lexNum cs = do
               toks <- lexer rest
               return $ LIT (read num) : toks
    where (num,rest) = span isDigit cs

(<:>) :: a -> Maybe [a] -> Maybe [a]
a <:> x = do
            as <- x
            return $ a : as

lexAlpha cs = let (as,rest) = span isAlpha cs in
 case as of
      "mov"    -> MOV <:> lexer rest
      "store"  -> STORE <:> lexer rest
      "add"    -> ADD <:> lexer rest
      "sub"    -> SUB <:> lexer rest
      "mul"    -> MUL <:> lexer rest
      "neg"    -> NEGATE <:> lexer rest
      "equal"  -> EQUAL <:> lexer rest
      "not"    -> NOT <:> lexer rest
      "gthan"  -> GTHAN <:> lexer rest
      "jmp"    -> JMP <:> lexer rest
      "brz"    -> BRZ <:> lexer rest
      "brnz"   -> BRNZ <:> lexer rest
      "read"   -> READ <:> lexer rest
      "write"  -> WRITE <:> lexer rest
      "FP"     -> FPtok <:> lexer rest
      "SP"     -> SPtok <:> lexer rest
      "BP"     -> BPtok <:> lexer rest
      "call"   -> CALL <:> lexer rest
      "ret"    -> RET <:> lexer rest
      "exit"   -> EXIT <:> lexer rest
      "pop"    -> POP <:> lexer rest
      "push"   -> PUSH <:> lexer rest
      "pushl"  -> PUSHL <:> lexer rest
      ";"      -> SEMICOL <:> lexer rest
      ('R':xs) -> REG name <:> lexer rs
                    where (name,rs) = consumeAlphaNum "" (xs ++ rest)
      _        -> error $ "What? " ++ as

consumeAlphaNum acc []                 = (reverse acc,[])
consumeAlphaNum acc (c:cs) | isAlpha c = consumeAlphaNum (c:acc) cs
                           | isDigit c = consumeAlphaNum (c:acc) cs
                           | otherwise = (reverse acc,c:cs)

consumeLine []           = Just []
consumeLine ('/':'/':cs) = lexer cs
consumeLine (c:cs)       = consumeLine cs

t2 = "read SP; \n write SP; \n read SP; \n write SP; \n read SP; \n write SP;"

foobar = "\tmov R0 #99;\n\tmov Rx R0;0:\tmov R1 #0;\tsub R2 Rx R1;\tbrnz R2 #2;\tmov R2 #0;\tjmp #3;2:\tmov R2 #1;3: \tbrz R2 #1;\tmov R3 #1;\tsub R4 Rx R3;\tmov Rx R4;\tjmp #0;1:"

--
-- Parser for ThreeAddr follows.
--

-- This is the main parser function
parse3addr :: [Token] -> Maybe [ThreeAddr]
parse3addr []     = Nothing
parse3addr (t:ts) = case t of
  MOV    -> do
              (r,a,ts') <- twoaddress ts
              rest      <- parse3addr ts'
              return $ Mov r a : rest
  LOAD   -> do
              (r1,r2,ts') <- tworegister ts
              rest        <- parse3addr ts'
              return $ Load r1 r2 : rest
  STORE  -> do
              (r1,r2,ts') <- tworegister ts
              rest        <- parse3addr ts'
              return $ Store r1 r2 : rest
  ADD    -> do
              (r,a1,a2,ts') <- threeaddress ts
              rest          <- parse3addr ts'
              return $ Add r a1 a2 : rest
  SUB    -> do
              (r,a1,a2,ts') <- threeaddress ts
              rest          <- parse3addr ts'
              return $ Sub r a1 a2 : rest
  MUL    -> do
              (r,a1,a2,ts') <- threeaddress ts
              rest          <- parse3addr ts'
              return $ Mul r a1 a2 : rest
  NEGATE -> do
              (r,a,ts') <- twoaddress ts
              rest      <- parse3addr ts'
              return $ Negate r a : rest
  EQUAL  -> do
              (r,a1,a2,ts') <- threeaddress ts
              rest          <- parse3addr ts'
              return $ Equal r a1 a2 : rest
  NOT    -> do
              (r,a,ts') <- twoaddress ts
              rest      <- parse3addr ts'
              return $ LogNot r a : rest
  GTHAN  -> do
              (r,a1,a2,ts') <- threeaddress ts
              rest          <- parse3addr ts'
              return $ GThan r a1 a2 : rest
  JMP    -> do
              (a,ts') <- oneaddress ts
              rest    <- parse3addr ts'
              return $ Jmp a : rest
  BRZ    -> do
              (r,a,ts') <- twoaddress ts
              rest      <- parse3addr ts'
              return $ BrZ r a : rest
  BRNZ   -> do
              (r,a,ts') <- twoaddress ts
              rest      <- parse3addr ts'
              return $ BrNZ r a : rest
  LIT i  -> do
              ts'  <- colon ts
              rest <- parse3addr ts'
              return $ Label i : rest              
  READ   -> do
              (r,ts') <- parseReg ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ Read r : rest
  WRITE  -> do
              (a,ts') <- parseArg ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ Write a : rest
  CALL   -> do
              (a,ts') <- parseArg ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ Call a : rest
  RET    -> do
              rest    <- parse3addr ts
              return $ Ret : rest
  EXIT   -> do
              rest    <- parse3addr ts
              return $ Exit : rest
  PUSH  -> do
              (a,ts') <- parseArg ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ Push a : rest
  PUSHL -> do
              (a,ts') <- parseLit ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ PushLabel a : rest
  POP  -> do
              (r,ts') <- parseReg ts
              ts''    <- semicolon ts'
              rest    <- parse3addr ts''
              return $ Pop r : rest
  ENDOFINPUT -> case ts of
                     [] -> Just [] -- nothing more to parse
                     _  -> Nothing -- something unrecognized at the end.
  SEMICOL    -> parse3addr ts
  hmmm    -> error $ "WTF: " ++ show hmmm

parseArg :: [Token] -> Maybe (Arg, [Token])
parseArg (REG x : ts) = Just (Immediate (Reg x),ts)
parseArg (SPtok : ts) = Just (Immediate SP,ts)
parseArg (FPtok : ts) = Just (Immediate SP,ts)
parseArg (BPtok : ts) = Just (Immediate SP,ts)
parseArg (LIT i : ts) = Just (Literal i,ts)
parseArg _            = Nothing

parseLit (LIT i : ts) = Just (i, ts)
parseLit _            = Nothing

parseReg :: [Token] -> Maybe (Register, [Token])
parseReg (REG x : ts) = Just (Reg x, ts)
parseReg (FPtok : ts) = Just (FP,ts)
parseReg (SPtok : ts) = Just (SP,ts)
parseReg (BPtok : ts) = Just (BP,ts)
parseReg _            = Nothing

semicolon :: [Token] -> Maybe [Token]
semicolon (SEMICOL : ts) = Just ts
semincolon _             = Nothing

colon :: [Token] -> Maybe [Token]
colon (COLON : ts) = Just ts
colon _            = Nothing

oneaddress :: [Token] -> Maybe (Arg,[Token])
oneaddress ts = do
  (a,ts') <- parseArg ts
  ts''    <- semicolon ts'
  return (a,ts'')

twoaddress :: [Token] -> Maybe (Register,Arg,[Token])
twoaddress ts = do
  (r,ts1) <- parseReg ts
  (a,ts2) <- parseArg ts1
  ts3     <- semicolon ts2
  return $ (r,a,ts3)

threeaddress :: [Token] -> Maybe (Register,Arg,Arg,[Token])
threeaddress ts = do
  (r,ts1)  <- parseReg ts
  (a1,ts2) <- parseArg ts1
  (a2,ts3) <- parseArg ts2
  ts4      <- semicolon ts3
  return $ (r,a1,a2,ts4)

tworegister :: [Token] -> Maybe (Register, Register, [Token])
tworegister ts = do 
  (r1,ts1) <- parseReg ts
  (r2,ts2) <- parseReg ts1
  ts3      <- semicolon ts2
  return (r1,r2,ts3)

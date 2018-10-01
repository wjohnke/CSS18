module BasicBlocks where

import ThreeAddrSyntax

-- ***********************************
--
-- For definitions, wikipedia is just fine:
--     https://en.wikipedia.org/wiki/Basic_block
-- Collberg's slides on this are fine, too:
--     https://www2.cs.arizona.edu/~collberg/Teaching/453/2009/Handouts/Handout-15.pdf
--

-- ***********************************
-- Calculating Basic Blocks.
--     There are two kinds of ThreeAddr instructions: "control-flow" and "regular"

-- ***********************************
-- Q: What are the "control-flow" instructions?

controlflow :: ThreeAddr -> Bool
controlflow (Jmp _)    = True
controlflow (BrZ _ _)  = True
controlflow (BrNZ _ _) = True
controlflow (Call _)   = True
controlflow Ret        = True
controlflow Exit       = True
controlflow _          = False

-- ***********************************
-- Q: What are the basic blocks in the following example?

-- ***
-- *** Here is the ThreeAddr code for "foobar.imp":
-- ***
-- 0:	mov R0 #99;
-- 	mov Rx R0;
-- 1:	mov R1 #0;
-- 	sub R2 Rx R1;
-- 	brnz R2 #3;
-- 	mov R2 #0;
-- 	jmp #4;
-- 3:	mov R2 #1;
-- 4:	brz R2 #2;
-- 	mov R3 #1;
-- 	sub R4 Rx R3;
-- 	mov Rx R4;
-- 	jmp #1;
-- 2:	exit;

-- ***
-- *** Here's the above code separated into basic blocks:
-- ***
-- 0:	mov R0 #99;
-- 	mov Rx R0;
--
-- 1:	mov R1 #0;
-- 	sub R2 Rx R1;
-- 	brnz R2 #3;
--
-- 	mov R2 #0;
-- 	jmp #4;
--
-- 3:	mov R2 #1;
--
-- 4:	brz R2 #2;
--
-- 	mov R3 #1;
-- 	sub R4 Rx R3;
-- 	mov Rx R4;
-- 	jmp #1;
--
-- 2:	exit;



-- ***********************************
-- Representing ThreeAddr Basic Blocks.

label (Label _) = True
label _         = False

blockify :: [ThreeAddr] -> [ThreeAddr] -> [[ThreeAddr]]
blockify [] acc       = []
blockify (tc:tcs) acc | controlflow tc = (reverse $ tc : acc) : blockify tcs []
                      | label tc       = case acc of
                                              [] -> blockify tcs [tc]
                                              _  -> reverse acc : blockify tcs [tc]
                      | otherwise      = blockify tcs (tc : acc)
                                                                

-- In Main.hs, we defined the following function to test blockify:

-- bb :: FilePath -> IO [[ThreeAddr]]
-- bb imp = do
--   impprog <- parseImp imp
--   let ThreeAddrProg tac = runM $ compileImp impprog
--   return $ blockify tac []


-- ***
-- *** Here's the output of bb for foobar.imp:
-- ***
-- [[0:,mov R0 #99,mov Rx R0],
--  [1:,mov R1 #0,sub R2 Rx R1,brnz R2 #3],
--  [mov R2 #0,jmp #4],
--  [3:,mov R2 #1],
--  [4:,brz R2 #2],
--  [mov R3 #1,sub R4 Rx R3,mov Rx R4,jmp #1],
--  [2:,exit]]

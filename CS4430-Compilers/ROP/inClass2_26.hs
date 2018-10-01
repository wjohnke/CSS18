
newInstrLabel :: StateT (Int, Int) Identity Int --This is a monad
newInstrLabel= do
	(bct,ict) <- get
	put (bct,ict+1)
	return ict


newBlockLabel :: StateT (Int, Int) Identity Int --This is a monad
newBlockLabel= do
	(bct,ict) <- get
	put (bct+1,ict)
	return bct

	
	
type T = StateT (Int, Int) Identity

enblockT :: [[ThreeAddr]] -> T [Block]
enblockT []               = return []
enblockT (tacb:tacbs)     = do
	b1      <- newBlockLabel
	ltacb   <- helpT tacb
	ltacbs  <-enblockT tacbs
	return ((b1,ltacb):ltacbs)
	
	
helpT :: [ThreeAddr]  -> T [(Int,ThreeAddr)]
helpT []              = return []
helpT (tac:tacs)      = do
	il     <- newInstrLabel
	ltacs  <- helpT tacs
	return ((il,tac):ltacs)
-- |
-- Module      : Extern.Maple
-- Description : Rudimentary interface to Maple
-- 
-- This module provides a very elementary interface to the @maple@
-- executable.

module Extern.Maple
    where

import Control.Monad (void)
import System.Process
import System.IO


data MapleInterp = MapleInterp { hin   :: Handle
                               , hout  :: Handle
                               , hproc :: ProcessHandle
                               }

-- | Create new instance of the maple interpreter. The @maple@
-- executable must be available on the executing system (and must
-- be able to obtain a license, if necessary).
newMaple :: IO MapleInterp
newMaple = do
    (Just hi, Just ho, _, hp) <-
        createProcess (proc "maple" ["-q"]) { std_in  = CreatePipe
                                            , std_out = CreatePipe }
    --
    mapM_ (flip hSetBuffering LineBuffering) [hi, ho]
    return $ MapleInterp { hin = hi, hout = ho, hproc = hp }

-- | Quits a maple interpreter and waits for its termination.
-- The passed 'MapleInterp' should not be used afterwards.
closeMaple :: MapleInterp -> IO ()
closeMaple maple = do
    hPutStrLn (hin maple) "quit"
    hClose (hin maple)
    hClose (hout maple)
    void $ waitForProcess (hproc maple)

-- | Feed input to a maple interpreter.
putMaple :: MapleInterp -> String -> IO ()
putMaple = hPutStrLn . hin

-- | Read one "chunk" of maple output (split at linebreaks);
-- we consider the end of each chunk to be three empty lines
-- (which are /not/ part of the returned list).
-- For example, running
-- 
-- @
--   do m <- 'newMaple'
--      'putMaple' m "41 + 1; printf(\\"\\\\n\\\\n\\");"
--      res <- 'getMaple' m
--      return (res == ["42"])
-- @
-- 
-- in the @IO@ monad should yield @True@.
getMaple :: MapleInterp -> IO [String]
getMaple maple = getLines
    where
        nEmpty   = 3
        getL     = hGetLine (hout maple)
        getLines = getLinesStop nEmpty
        
        getLinesStop :: Int -> IO [String]
        getLinesStop 0 = return []
        getLinesStop k = do
            l  <- getL
            if l == ""
            then getLinesStop (k-1)
            else do
                ls <- getLines
                return $ replicate (nEmpty - k) "" ++ [l] ++ ls

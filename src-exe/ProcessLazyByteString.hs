module ProcessLazyByteString (readProcessWithExitCode) where

import           Control.Concurrent.Async      (wait, withAsync)
import qualified Control.Exception             as E
import           Control.Monad                 (unless)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString (..),
                                                       defaultChunkSize)
import           Foreign.C.Error               (Errno (..), ePIPE)
import qualified GHC.IO.Exception              as GHC
import           System.Exit                   (ExitCode)
import           System.IO                     (Handle, hClose)
import qualified System.Process                as Proc

readProcessWithExitCode
    :: String           -- ^ Command
    -> [String]         -- ^ Arguments
    -> BS.ByteString    -- ^ Stdin
    -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
readProcessWithExitCode cmd args = readProcessImpl (Proc.proc cmd args)

readProcessImpl
    :: Proc.CreateProcess
    -> BS.ByteString
    -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
readProcessImpl cp input =
    Proc.withCreateProcess cp' $ \mi mo me ph -> case (mi, mo, me) of
        (Just inh, Just outh, Just errh) ->
            -- spawn workers to read stdout and stderr
            withAsync (getLBSContents outh) $ \outA ->
            withAsync (getLBSContents errh) $ \errA -> do
                -- write the input
                unless (BS.null input) $ BS.hPutStr inh input
                ignoreSigPipe $ hClose inh

                -- wait for the output
                out <- wait outA
                err <- wait errA

                -- wait for the process
                ec <- Proc.waitForProcess ph

                return (ec, out, err)

        (Nothing,_,_) -> fail "readProcessWithExitCode: Failed to get a stdin handle."
        (_,Nothing,_) -> fail "readProcessWithExitCode: Failed to get a stdout handle."
        (_,_,Nothing) -> fail "readProcessWithExitCode: Failed to get a stderr handle."

  where
    cp' :: Proc.CreateProcess
    cp' = cp
        { Proc.std_in  = Proc.CreatePipe
        , Proc.std_out = Proc.CreatePipe
        , Proc.std_err = Proc.CreatePipe
        }

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = E.handle $ \e -> case e of
    GHC.IOError { GHC.ioe_type  = GHC.ResourceVanished, GHC.ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> E.throwIO e

getLBSContents :: Handle -> IO LBS.ByteString
getLBSContents = hGetContentsN LBS.defaultChunkSize

-- No unsafeInterleaveIO
hGetContentsN :: Int -> Handle -> IO LBS.ByteString
hGetContentsN k h = loop `E.finally` hClose h where
    loop = do
        c <- BS.hGetSome h k -- only blocks if there is no data available
        if BS.null c
        then return LBS.Empty
        else LBS.Chunk c <$> loop

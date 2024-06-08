{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module CText (
    -- * CText
    CText (..),
    CPiece (..),
    T (..), fromT, fromText,
    -- ** Colorify
    recolorify, colorifyText, colorifyStr,
    underline, emphasise,
    -- * CWriter
    CWriter,
    runCWriterIO,
    MonadCWriter (..),
    UseColors (..),
    UseAscii (..),
    -- * Underline
    haveUnderlineSupport,
    -- * Re-exports
    module System.Console.ANSI,
    ) where

import           Control.Monad              (ap, unless)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable              (for_)
import qualified Data.List                  as L
import           Data.Monoid                (Endo (..))
import           Data.Semigroup             (Semigroup (..))
import           Data.String                (IsString (..))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           GHC.IO.Encoding.Types      (textEncodingName)
import           System.Console.ANSI
import           System.IO                  (hGetEncoding, stdout)

haveUnderlineSupport :: Bool
#if defined(UNDERLINE_SUPPORT)
haveUnderlineSupport = True
#else
haveUnderlineSupport = False
#endif

data CPiece = CPiece !T [SGR]
  deriving (Eq, Show)

data T
    = T !T.Text
    | Vert -- vertical
    | Junc -- junction
    | Corn -- corner
    | Spac -- space
    | Rest -- "ellipsis"
  deriving (Eq, Show)

newtype CText = CText [CPiece]
  deriving (Eq, Show)

instance IsString CText where
    fromString s
        | null s    = mempty
        | otherwise = CText [CPiece (T (fromString s)) []]

instance Semigroup CText where
    CText xs <> CText ys = CText (xs <> ys)

instance Monoid CText where
    mempty  = CText []
    mappend = (<>)

fromText :: T.Text -> CText
fromText t = CText [CPiece (T t) []]

fromT :: T -> CText
fromT t = CText [CPiece t []]

colorifyStr :: Color -> String -> CText
colorifyStr c t = CText [CPiece (T (T.pack t)) [SetColor Foreground Vivid c]]

colorifyText :: Color -> T.Text -> CText
colorifyText c t = CText [CPiece (T t) [SetColor Foreground Vivid c]]

recolorify :: Color -> CText -> CText
recolorify c (CText xs) = CText
    [ CPiece t (SetColor Foreground Vivid c : sgr)
    | CPiece t sgr' <- xs
    , let sgr = filter notSetColor sgr'
    ]
  where
    notSetColor SetColor {} = False
    notSetColor _           = True

-- | We decide to bold, we could do something else to.
emphasise :: CText -> CText
emphasise (CText xs) = CText
    [ CPiece t (SetConsoleIntensity BoldIntensity : sgr)
    | CPiece t sgr <- xs
    ]

underline :: CText -> CText
underline (CText xs) | haveUnderlineSupport  = CText
    [ CPiece t (SetUnderlining SingleUnderline : sgr)
    | CPiece t sgr <- xs
    ]
underline x = x

-- | Colored writer (list is lines)
newtype CWriter a = CWriter { unCWriter :: Endo [CText] -> (Endo [CText], a) }
  deriving Functor

class Monad m => MonadCWriter m where
    putCTextLn :: CText -> m ()

instance MonadCWriter CWriter where
    putCTextLn t = CWriter $ \l -> (l <> Endo (t :), ())

instance MonadCWriter m => MonadCWriter (StateT s m) where
    putCTextLn = lift . putCTextLn

instance Applicative CWriter where
    pure  = return
    (<*>) = ap

instance Monad CWriter where
    return x = CWriter $ \ls -> (ls, x)

    m >>= k = CWriter $ \ls0 ->
        let (ls1, x) = unCWriter m ls0
        in unCWriter (k x) ls1

data UseColors = ColorsNever | ColorsAuto | ColorsAlways
  deriving (Eq, Show)

data UseAscii = UseAscii | UseUnicode | UseAsciiAuto
  deriving (Eq, Show)

runCWriterIO :: UseColors -> UseAscii -> CWriter () -> IO ()
runCWriterIO ColorsNever  useAscii m = do
    useAscii' <- shouldUseAscii useAscii
    runCWriterIONoColors useAscii' m
runCWriterIO ColorsAlways useAscii m = do
    useAscii' <- shouldUseAscii useAscii
    runCWriterIOColors useAscii' m
runCWriterIO ColorsAuto   useAscii m = do
    useAscii' <- shouldUseAscii useAscii
    supports <- hSupportsANSIColor stdout
    if supports
    then runCWriterIOColors   useAscii' m
    else runCWriterIONoColors useAscii' m

-- TODO: check environment variables?
shouldUseAscii :: UseAscii -> IO Bool
shouldUseAscii UseAscii     = return True
shouldUseAscii UseUnicode   = return False
shouldUseAscii UseAsciiAuto = do
    e <- hGetEncoding stdout
    return $ not $ fmap (L.isPrefixOf "UTF" . textEncodingName) e == Just True

putT :: Bool -> T -> IO ()
putT _     (T t) = T.putStr t
-- https://en.wikipedia.org/wiki/Box-drawing_character
putT False Vert  = T.putStr " \x2502  "
putT False Junc  = T.putStr " \x251c\x2500 "
putT False Corn  = T.putStr " \x2514\x2500 "
putT False Rest  = T.putStr " \x2504\x2504"
-- ascii
putT True  Vert  = T.putStr " |  "
putT True  Junc  = T.putStr " +- "
putT True  Corn  = T.putStr " +- "
putT True  Rest  = T.putStr " ..."
-- space is just space
putT _     Spac  = T.putStr "    "

runCWriterIOColors :: Bool -> CWriter () -> IO ()
runCWriterIOColors useAscii (CWriter f) =
    for_ (appEndo (fst (f mempty)) []) $ \(CText l) -> do
        for_ l $ \(CPiece t sgr) -> do
            unless (null sgr) $ setSGR sgr
            putT useAscii t
            unless (null sgr) $ setSGR []
        putChar '\n'

runCWriterIONoColors :: Bool -> CWriter () -> IO ()
runCWriterIONoColors useAscii (CWriter f) =
    for_ (appEndo (fst (f mempty)) []) $ \(CText l) -> do
        for_ l $ \(CPiece t _) -> putT useAscii t
        putChar '\n'

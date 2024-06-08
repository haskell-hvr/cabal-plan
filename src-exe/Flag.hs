{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Because 'Flag' constructor isn't exposed,
-- we have to explicitly 'toFlag' and 'fromFlag'.
-- That way it's less likely we mix up bare Booleans.
module Flag (
    -- * Flag
    Flag, toFlag, fromFlag,
    -- * HasDefault
    HasDefault,
    -- * optparse-applicative
    showHide,
    switchM,
    ) where

import           Control.Applicative ((<|>))
import           Data.Semigroup (Semigroup (..))
import           Data.Singletons.Bool
import qualified Options.Applicative as O

-------------------------------------------------------------------------------
-- Flag
-------------------------------------------------------------------------------

newtype Flag t = Flag Bool

toFlag :: t -> Bool -> Flag t
toFlag _ = Flag

fromFlag :: t -> Flag t -> Bool
fromFlag _ (Flag b) = b

-------------------------------------------------------------------------------
-- HasDefault
-------------------------------------------------------------------------------

-- | Default value.
--
-- With 'DeriveAnyClass' one could write
--
-- @
-- data MyOpt = MyOpt deriving (HasDefault 'True)
-- @
--
class SBoolI def => HasDefault (def :: Bool) t | t -> def

def :: forall t def. HasDefault def t => t -> Flag t
def t = toFlag t (reflectBool (P :: P def))

data P (def :: Bool) = P

-------------------------------------------------------------------------------
-- optparse-applicative
-------------------------------------------------------------------------------

showHide :: HasDefault def t => t -> String -> String -> O.Parser (Flag t)
showHide t n d =
    O.flag' (toFlag t True) (O.long ("show-" ++ n) Data.Semigroup.<> O.help d)
    <|> O.flag' (toFlag t False) (O.long ("hide-" ++ n))
    <|> pure (def t)

switchM :: HasDefault 'False t => t -> String -> String -> O.Parser (Flag t)
switchM t n d = fmap (toFlag t) $ O.switch $ O.long n <> d' where
    d' | null d    = mempty
       | otherwise = O.help d

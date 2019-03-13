{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Because 'TaggedBool' constructor isn't exposed,
-- we have to explicitly 'tagBool' and 'untagBool'.
-- That way it's less likely we mix up bare Booleans.
module TaggedBool (
    -- * TaggedBool
    TaggedBool, tagBool, untagBool,
    -- * HasDefault
    HasDefault,
    -- * optparse-applicative
    showHide,
    switchM,
    ) where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Applicative ((<|>))
import           Data.Semigroup (Semigroup (..))
import           Data.Singletons.Bool
import qualified Options.Applicative as O

-------------------------------------------------------------------------------
-- TaggedBool
-------------------------------------------------------------------------------

newtype TaggedBool t = TaggedBool Bool

tagBool :: t -> Bool -> TaggedBool t
tagBool _ = TaggedBool

untagBool :: t -> TaggedBool t -> Bool
untagBool _ (TaggedBool b) = b

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

def :: forall t def. HasDefault def t => t -> TaggedBool t
def t = tagBool t (reflectBool (P :: P def))

data P (def :: Bool) = P

-------------------------------------------------------------------------------
-- optparse-applicative
-------------------------------------------------------------------------------

showHide :: HasDefault def t => t -> String -> String -> O.Parser (TaggedBool t)
showHide t n d =
    O.flag' (tagBool t True) (O.long ("show-" ++ n) <> O.help d)
    <|> O.flag' (tagBool t False) (O.long ("hide-" ++ n))
    <|> pure (def t)

switchM :: HasDefault 'False t => t -> String -> String -> O.Parser (TaggedBool t)
switchM t n d = fmap (tagBool t) $ O.switch $ O.long n <> d' where
    d' | null d    = mempty
       | otherwise = O.help d

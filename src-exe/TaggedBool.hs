-- | Because 'TaggedBool' constructor isn't exposed,
-- we have to explicitly 'tagBool' and 'untagBool'.
-- That way it's less likely we mix up bare Booleans.
module TaggedBool (TaggedBool, tagBool, untagBool) where

newtype TaggedBool t = TaggedBool Bool

tagBool :: t -> Bool -> TaggedBool t
tagBool _ = TaggedBool

untagBool :: t -> TaggedBool t -> Bool
untagBool _ (TaggedBool b) = b

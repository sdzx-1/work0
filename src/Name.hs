{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Name where

import Control.Effect.Fresh
import qualified Data.Char as Char
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

-- | The type of variable names.
data Name
  = Name Text
  | I Int
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString

-- | Generate a fresh (unused) name for use in synthesized variables/closures/etc.
gensym :: Has Fresh sig m => m Name
gensym = I <$> fresh

-- | Construct a 'Name' from a 'Text'.
name :: Text -> Name
name = Name

isGenerated :: Name -> Bool
isGenerated (I _) = True
isGenerated _ = False

-- | Construct a 'Name' from an 'Int'. This is suitable for automatic generation, e.g. using a Fresh effect, but should not be used for human-generated names.
nameI :: Int -> Name
nameI = I

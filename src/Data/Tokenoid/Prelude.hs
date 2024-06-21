{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Tokenoid.Prelude
  ( module X,
    cs,
  )
where

import Control.Lens as X (Lens', lens, use, (+=), (.~))
import Protolude as X
import Protolude.Conv (Leniency (Lenient), StringConv (strConv))

cs :: forall a b. (StringConv a b) => a -> b
cs = strConv Lenient
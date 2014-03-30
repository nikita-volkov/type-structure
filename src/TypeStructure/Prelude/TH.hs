module TypeStructure.Prelude.TH
(
  module Exports,

  purify,
  tryToReify,
  whenNoInstance,
)
where

import TypeStructure.Prelude.Basic
import Language.Haskell.TH as Exports


purify :: Q a -> a
purify = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

-- |
-- Only checks the instances in scope of the calling site,
-- it will not detect the declared instances, if they are not imported.
whenNoInstance :: Monoid a => Name -> [Type] -> Q a -> Q a
whenNoInstance name types f = do
  z <- recover (return False) (isInstance name types)
  if z
    then return mempty
    else f


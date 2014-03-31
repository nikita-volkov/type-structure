{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeStructure.THTest where

import Test.Framework
import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Transformers
import TypeStructure.Prelude.TH
import TypeStructure.TH


data List a = Cons a (List a) | Nil

test_analyzeReferredTypesOnRecursiveType = do
  let r = $(stringE . show =<< execStateT (analyzeReferredTypes ''List) [])
  assertEqual "[Left TypeStructure.THTest.List]" r

{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TypeStructureTest where

import Test.Framework
import TypeStructure.Prelude.Basic
import TypeStructure.Prelude.Data
import TypeStructure


test_differentStructuresDontEqual = do
  assertNotEqual (graph (undefined :: Int16)) (graph (undefined :: Int8))

test_sameStructuresDoEqual = do
  assertEqual (graph (undefined :: Int)) (graph (undefined :: Int))

test_differentStructuresProduceDifferentHashes = do
  assertNotEqual (hash $ graph (undefined :: Int16)) (hash $ graph (undefined :: Int8))

{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import TypeStructure.Prelude.Basic

import {-@ HTF_TESTS @-} TypeStructureTest
import {-@ HTF_TESTS @-} TypeStructure.THTest

main = htfMain $ htf_thisModulesTests : htf_importedTests

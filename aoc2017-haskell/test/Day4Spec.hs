module Day4Spec (
  testPassphraseValidation,
  testAnagramPassphraseValidation
) where

import Test.Tasty
import Test.Tasty.HUnit

import Day4

testPassphraseValidation :: TestTree
testPassphraseValidation =
    testGroup "Testing validation for isValidPassphrase" $ [
      testPassphrase "aa bb cc dd ee"  True,
      testPassphrase "aa bb cc dd aa"  False,
      testPassphrase "aa bb cc dd aaa" True
    ]
  where
    testPassphrase passphrase result =
      testCase passphrase $ isValidPassphrase passphrase @?= result

testAnagramPassphraseValidation :: TestTree
testAnagramPassphraseValidation =
    testGroup "Testing validation for isValidAnagramPassphrase" $ [
      testPassphrase "abcde fghij"              True,
      testPassphrase "abcde xyz ecdab"          False,
      testPassphrase "a ab abc abd abf abj"     True,
      testPassphrase "iiii oiii ooii oooi oooo" True,
      testPassphrase "oiii ioii iioi iiio"      False
    ]
  where
    testPassphrase passphrase result =
      testCase passphrase $ isValidAnagramPassphrase passphrase @?= result
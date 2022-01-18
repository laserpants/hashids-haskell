{-# LANGUAGE TemplateHaskell #-}

module Test.Web.Hashids.Property
    ( tests
    ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (void)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Hedgehog              (Property, PropertyT, assert,
                                        checkParallel, discover, eval, forAll,
                                        property, tripping, withTests)
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import           Web.Hashids

import           Test.Web.Hashids.Gen  (genAlphabet, genHashidsContext,
                                        genHashidsContextWithAlphabet,
                                        genHashidsContextWithMinLen,
                                        genHashidsContextWithSalt,
                                        genMinHashLength, genSalt)

prop_tripping :: Property
prop_tripping =
    withTests 500 $ property $ do
        context <- forAll genHashidsContext
        x       <- forAll (Gen.integral Range.constantBounded)
        tripping x (encode context) (decode context)

prop_trippingUsingSalt :: Property
prop_trippingUsingSalt =
    withTests 500 $ property $ do
        salt <- forAll genSalt
        x    <- forAll (Gen.integral Range.constantBounded)
        tripping x (encodeUsingSalt salt) (decodeUsingSalt salt)

prop_encNotEqualToPlain :: Property
prop_encNotEqualToPlain =
    withTests 500 $ property $ do
        context <- forAll genHashidsContext
        x       <- forAll (Gen.integral Range.constantBounded)
        let hashids     = encode context x
            xByteString = C.pack $ show x
        assert $ hashids /= xByteString

prop_encDifferentSaltsNotEqual :: Property
prop_encDifferentSaltsNotEqual =
    withTests 500 $ property $ do
        salt1    <- forAll genSalt
        salt2    <- forAll genSalt
        context1 <- forAll (genHashidsContextWithSalt salt1)
        context2 <- forAll (genHashidsContextWithSalt salt2)
        x        <- forAll (Gen.integral Range.constantBounded)
        let hashids1 = encode context1 x
            hashids2 = encode context2 x
        assert $ hashids1 /= hashids2

prop_encOnlyConsistsOfAlphabet :: Property
prop_encOnlyConsistsOfAlphabet =
    withTests 500 $ property $ do
        alphabet <- forAll genAlphabet
        context  <- forAll (genHashidsContextWithAlphabet alphabet)
        x        <- forAll (Gen.integral Range.constantBounded)
        let hashids = encode context x
        assert $ not $ C.any ((flip notElem) alphabet) hashids

prop_encMinLength :: Property
prop_encMinLength =
    withTests 500 $ property $ do
        minHashLength <- forAll genMinHashLength
        context       <- forAll (genHashidsContextWithMinLen minHashLength)
        x             <- forAll (Gen.integral Range.constantBounded)
        let hashids = encode context x
        assert $ BS.length hashids >= minHashLength

encodedRange :: Int -> Range.Range Int
encodedRange minLength = Range.exponentialFrom minLength minLength 10000

assertDecodeReturns :: HashidsContext -> BS.ByteString -> PropertyT IO ()
assertDecodeReturns context encoded =
    -- `length` evaluates the output to NF, not just WHNF.
    void $ eval $ length $ decode context encoded

-- | Check that `decode` is a complete function, testing completely arbitrary
-- inputs.
prop_decComplete :: Property
prop_decComplete =
    withTests 500 $ property $ do
        context <- forAll genHashidsContext
        encoded <- forAll (Gen.bytes $ encodedRange 0)
        assertDecodeReturns context encoded

-- | Check that `decode` is a complete function, specifically testing inputs
-- that look like valid encodings.
prop_decCompleteLooksValid :: Property
prop_decCompleteLooksValid =
    withTests 500 $ property $ do
        salt <- forAll genSalt
        minLen <- forAll genMinHashLength
        alphabet <- forAll genAlphabet
        let context = createHashidsContext salt minLen alphabet
            charGen = Gen.element alphabet
        encoded <- C.pack <$> forAll (Gen.list (encodedRange minLen) charGen)
        assertDecodeReturns context encoded

tests :: IO Bool
tests = and <$> sequence
    [ checkParallel $$(discover)
    ]

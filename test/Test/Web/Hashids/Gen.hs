module Test.Web.Hashids.Gen
    ( genAlphabet
    , genMinHashLength
    , genSalt
    , genHashidsContext
    , genHashidsContextWithAlphabet
    , genHashidsContextWithMinLen
    , genHashidsContextWithSalt
    ) where

import           Control.Applicative ((<$>))
import           Data.ByteString     (ByteString)
import           Data.List           (nub)
import           Data.Set            (toList)
import           Hedgehog            (Gen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Web.Hashids

genAlphabet :: Gen String
genAlphabet =
    nub
        <$> toList
        <$> Gen.set (Range.linear 16 62) Gen.alphaNum

genMinHashLength :: Gen Int
genMinHashLength = Gen.integral (Range.linear 1 1024)

genSalt :: Gen ByteString
genSalt = Gen.bytes (Range.linear 1 1024)

genHashidsContext :: Gen HashidsContext
genHashidsContext = do
    createHashidsContext
        <$> genSalt
        <*> genMinHashLength
        <*> genAlphabet

genHashidsContextWithAlphabet :: String -> Gen HashidsContext
genHashidsContextWithAlphabet alphabet =
    createHashidsContext
        <$> genSalt
        <*> genMinHashLength
        <*> pure alphabet

genHashidsContextWithMinLen :: Int -> Gen HashidsContext
genHashidsContextWithMinLen minHashLength =
    createHashidsContext
        <$> genSalt
        <*> pure minHashLength
        <*> genAlphabet

genHashidsContextWithSalt :: ByteString -> Gen HashidsContext
genHashidsContextWithSalt salt =
    createHashidsContext
        <$> pure salt
        <*> genMinHashLength
        <*> genAlphabet

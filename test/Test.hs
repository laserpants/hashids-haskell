{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative       ((<$>))
import           Control.Monad             (forM_, unless, void)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (pack)
import           System.Exit               (exitFailure, exitSuccess)

import           Web.Hashids

import           Test.Web.Hashids.Property (tests)

pair :: [a] -> [(a, a)]
pair []       = []
pair [x]      = []
pair (x:y:xs) = (x, y):pair xs

hashidsContext :: HashidsContext
hashidsContext = hashidsMinimum "this is a salt" 10

enc :: String -> ByteString
enc = encodeList hashidsContext . read

match :: (String, String) -> Bool
match (numbers, hash) = enc numbers == pack hash

exit :: Bool -> IO ()
exit True = exitSuccess
exit _    = exitFailure

main :: IO ()
main = do
    -- Hedgehog property tests
    void tests

    -- Consistency test
    file <- readFile "test/testdata/testdata1.txt"
    forM_ (pair $ lines file) $ \(n,h) ->
        unless (enc n == pack h) $ do
            print (enc n, pack h)
            exit False
    exit True

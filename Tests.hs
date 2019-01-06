{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative   ((<$>))
import           Control.Monad         (forM_, unless)
import           Data.ByteString       (ByteString)
import           System.Exit           (exitFailure, exitSuccess)
import           Web.Hashids

import           Data.ByteString.Char8 (pack, unpack)

pair :: [a] -> [(a, a)]
pair []       = []
pair [x]      = []
pair (x:y:xs) = (x, y):pair xs

hashids = hashidsMinimum "this is a salt" 10

enc :: String -> ByteString
enc = encodeList hashids . read

match :: (String, String) -> Bool
match (numbers, hash) = enc numbers == pack hash

exit :: Bool -> IO ()
exit True = exitSuccess
exit _    = exitFailure

main = do
    file <- readFile "tests/testdata1.txt"
    forM_ (pair $ lines file) $ \(n,h) ->
        unless (enc n == pack h) $ do
            print (enc n, pack h)
            exit False
    exit True


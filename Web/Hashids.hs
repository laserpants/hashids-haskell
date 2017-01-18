{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This is a Haskell port of the Hashids library by Ivan Akimov.
--   This is /not/ a cryptographic hashing algorithm. Hashids is typically
--   used to encode numbers to a format suitable for appearance in places
--   like urls.
--
-- See the official Hashids home page: <http://hashids.org>
--
-- Hashids is a small open-source library that generates short, unique,
-- non-sequential ids from numbers. It converts numbers like 347 into
-- strings like @yr8@, or a list of numbers like [27, 986] into @3kTMd@.
-- You can also decode those ids back. This is useful in bundling several
-- parameters into one or simply using them as short UIDs.

module Web.Hashids
    ( HashidsContext
    -- * How to use
    -- $howto

    -- ** Encoding
    -- $encoding

    -- ** Decoding
    -- $decoding

    -- ** Randomness
    -- $randomness

    -- *** Repeating numbers
    -- $repeating

    -- *** Incrementing number sequence
    -- $incrementing

    -- ** Curses\! \#\$\%\@
    -- $curses

    -- * API
    , version
    -- ** Context object constructors
    , createHashidsContext
    , hashidsSimple
    , hashidsMinimum
    -- ** Encoding and decoding
    , encodeHex
    , decodeHex
    , encode
    , encodeList
    , decode
    -- ** Convenience wrappers
    , encodeUsingSalt
    , encodeListUsingSalt
    , decodeUsingSalt
    , encodeHexUsingSalt
    , decodeHexUsingSalt
    ) where

import Data.ByteString                   ( ByteString )
import Data.Foldable                     ( toList )
import Data.List                         ( (\\), nub, intersect, foldl' )
import Data.List.Split                   ( chunksOf )
import Data.Sequence                     ( Seq )
import Numeric                           ( showHex, readHex )

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C8
import qualified Data.Sequence           as Seq

-- $howto
--
-- Note that most of the examples on this page require the OverloadedStrings extension.

-- $encoding
--
-- Unless you require a minimum length for the generated hash, create a
-- context using 'hashidsSimple' and then call 'encode' and 'decode' with
-- this object.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Web.Hashids
-- >
-- > main :: IO ()
-- > main = do
-- >     let context = hashidsSimple "oldsaltyswedishseadog"
-- >     print $ encode context 42
--
-- This program will output
--
-- > "kg"
--
-- To specify a minimum hash length, use 'hashidsMinimum' instead.
--
-- > main = do
-- >     let context = hashidsMinimum "oldsaltyswedishseadog" 12
-- >     print $ encode context 42
--
-- The output will now be
--
-- > "W3xbdkgdy42v"
--
-- If you only need the context once, you can use one of the provided wrappers
-- to simplify things.
--
-- > main :: IO ()
-- > main = print $ encodeUsingSalt "oldsaltyswedishseadog" 42
--
-- On the other hand, if your implementation invokes the hashing algorithm
-- frequently without changing the configuration, it is probably better to
-- define partially applied versions of 'encode', 'encodeList', and 'decode'.
--
-- > import Web.Hashids
-- >
-- > context :: HashidsContext
-- > context = createHashidsContext "oldsaltyswedishseadog" 12 "abcdefghijklmnopqrstuvwxyz"
-- >
-- > encode'     = encode context
-- > encodeList' = encodeList context
-- > decode'     = decode context
-- >
-- > main :: IO ()
-- > main = print $ encode' 12345
--
-- Use a custom alphabet and 'createHashidsContext' if you want to make your
-- hashes \"unique\".
--
-- > main = do
-- >     let context = createHashidsContext "oldsaltyswedishseadog" 0 "XbrNfdylm5qtnP19R"
-- >     print $ encode context 1
--
-- The output is now
--
-- > "Rd"
--
-- To encode a list of numbers, use `encodeList`.
--
-- > let context = hashidsSimple "this is my salt" in encodeList context [0, 1, 2]
--
-- > "yJUWHx"

-- $decoding
--
-- Decoding a hash returns a list of numbers,
--
-- > let context = hashidsSimple "this is my salt"
-- >      hash = decode context "rD"        -- == [5]
--
-- Decoding will not work if the salt is changed:
--
-- > main = do
-- >     let context = hashidsSimple "this is my salt"
-- >         hash = encode context 5
-- >
-- >     print $ decodeUsingSalt "this is my pepper" hash
--
-- When decoding fails, the empty list is returned.
--
-- > []
--

-- $randomness
--
-- Hashids is based on a modified version of the Fisher-Yates shuffle. The
-- primary purpose is to obfuscate ids, and it is not meant for security
-- purposes or compression. Having said that, the algorithm does try to make
-- hashes unguessable and unpredictable. See the official Hashids home page
-- for details: <http://hashids.org>

-- $repeating
--
-- > let context = hashidsSimple "this is my salt" in encodeList context $ replicate 4 5
--
-- There are no repeating patterns in the hash to suggest that four identical
-- numbers are used:
--
-- > "1Wc8cwcE"
--
-- The same is true for increasing numbers:
--
-- > let context = hashidsSimple "this is my salt" in encodeList context [1..10]
--
-- > "kRHnurhptKcjIDTWC3sx"

-- $incrementing
--
-- > let context = hashidsSimple "this is my salt" in map (encode context) [1..5]
--
-- > ["NV","6m","yD","2l","rD"]

-- $curses
--
-- The algorithm tries to avoid generating common curse words in English by
-- never placing the following letters next to each other:
--
-- > c, C, s, S, f, F, h, H, u, U, i, I, t, T

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

{-# INLINE splitOn #-}
splitOn :: ByteString -> ByteString -> [ByteString]
splitOn = BS.splitWith . flip BS.elem

-- | Opaque data type with various internals required for encoding and decoding.
data HashidsContext = Context
    { guards        :: !ByteString
    , seps          :: !ByteString
    , salt          :: !ByteString
    , minHashLength :: !Int
    , alphabet      :: !ByteString }

-- | Hashids version number.
version :: String
version = "1.0.2"

-- | Create a context object using the given salt, a minimum hash length, and
--   a custom alphabet. If you only need to supply the salt, or the first two
--   arguments, use 'hashidsSimple' or 'hashidsMinimum' instead.
--
--   Changing the alphabet is useful if you want to make your hashes unique,
--   i.e., create hashes different from those generated by other applications
--   relying on the same algorithm.
createHashidsContext :: ByteString  -- ^ Salt
                     -> Int         -- ^ Minimum required hash length
                     -> String      -- ^ Alphabet
                     -> HashidsContext
createHashidsContext salt minHashLen alphabet
    | length uniqueAlphabet < minAlphabetLength
        = error $ "alphabet must contain at least " ++ show minAlphabetLength ++ " unique characters"
    | ' ' `elem` uniqueAlphabet
        = error "alphabet cannot contain spaces"
    | BS.null seps'' || fromIntegral (BS.length alphabet') / fromIntegral (BS.length seps'') > sepDiv
        = case sepsLength - BS.length seps'' of
            diff | diff > 0
                -> res (BS.drop diff alphabet') (seps'' `BS.append` BS.take diff alphabet')
            _   -> res alphabet' (BS.take sepsLength seps'')
    | otherwise = res alphabet' seps''
  where

    res ab _seps =
        let shuffled = consistentShuffle ab salt
            guardCount = ceiling (fromIntegral (BS.length shuffled) / guardDiv)
            context = Context
                { guards        = BS.take guardCount _seps
                , seps          = BS.drop guardCount _seps
                , salt          = salt
                , minHashLength = minHashLen
                , alphabet      = shuffled }

         in if BS.length shuffled < 3
                then context
                else context{ guards   = BS.take guardCount shuffled
                            , seps     = _seps
                            , alphabet = BS.drop guardCount shuffled }

    seps'  = C8.pack $ uniqueAlphabet `intersect` seps
    seps'' = consistentShuffle seps' salt

    sepsLength =
        case ceiling (fromIntegral (BS.length alphabet') / sepDiv) of
          1 -> 2
          n -> n

    uniqueAlphabet    = nub alphabet
    alphabet'         = C8.pack $ uniqueAlphabet \\ seps
    minAlphabetLength = 16
    sepDiv            = 3.5
    guardDiv          = 12
    seps              = "cfhistuCFHISTU"

defaultAlphabet :: String
defaultAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ "1234567890"

-- | Create a context object using the default alphabet and the provided salt,
--   without any minimum required length.
hashidsSimple :: ByteString       -- ^ Salt
              -> HashidsContext
hashidsSimple salt = createHashidsContext salt 0 defaultAlphabet

-- | Create a context object using the default alphabet and the provided salt.
--   The generated hashes will have a minimum length as specified by the second
--   argument.
hashidsMinimum :: ByteString      -- ^ Salt
               -> Int             -- ^ Minimum required hash length
               -> HashidsContext
hashidsMinimum salt minimum = createHashidsContext salt minimum defaultAlphabet

-- | Decode a hash generated with 'encodeHex'.
--
-- /Example use:/
--
-- > decodeHex context "yzgwD"
--
decodeHex :: HashidsContext     -- ^ A Hashids context object
          -> ByteString         -- ^ Hash
          -> String
decodeHex context hash = concatMap (drop 1 . flip showHex "") numbers
  where
    numbers = decode context hash

-- | Encode a hexadecimal number.
--
-- /Example use:/
--
-- > encodeHex context "ff83"
--
encodeHex :: HashidsContext     -- ^ A Hashids context object
          -> String             -- ^ Hexadecimal number represented as a string
          -> ByteString
encodeHex context str
    | not (all hexChar str) = ""
    | otherwise = encodeList context $ map go $ chunksOf 12 str
  where
    go str = let [(a,_)] = readHex ('1':str) in a
    hexChar c = c `elem` ("0123456789abcdefABCDEF" :: String)

-- | Decode a hash.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = decode context "rD"        -- == [5]
--
decode :: HashidsContext     -- ^ A Hashids context object
       -> ByteString         -- ^ Hash
       -> [Int]
decode ctx@Context{..} hash
    | BS.null hash = []
    | encodeList ctx res /= hash = []
    | otherwise = res
  where
    res = splitOn seps tail
            |> foldl' go ([], alphabet)
            |> fst
            |> reverse

    hashArray = splitOn guards hash
    alphabetLength = BS.length alphabet

    Just str@(lottery, tail) =
         BS.uncons $ hashArray !! case length hashArray of
            0 -> error "Internal error."
            2 -> 1
            3 -> 1
            _ -> 0

    prefix = BS.cons lottery salt

    go (xs, ab) ssh =
        let buffer = prefix `BS.append` ab
            ab'    = consistentShuffle ab buffer
         in (unhash ssh ab':xs, ab')

numbersHashInt :: [Int] -> Int
numbersHashInt xs = foldr ((+) . uncurry mod) 0 $ zip xs [100 .. ]

-- | Encode a single number.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = encode context 5        -- == "rD"
--
encode :: HashidsContext        -- ^ A Hashids context object
       -> Int                   -- ^ Number to encode
       -> ByteString
encode context n = encodeList context [n]

-- | Encode a list of numbers.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = encodeList context [2, 3, 5, 7, 11]          -- == "EOurh6cbTD"
--
encodeList :: HashidsContext    -- ^ A Hashids context object
           -> [Int]             -- ^ List of numbers
           -> ByteString
encodeList _ [] = error "encodeList: empty list"
encodeList Context{..} numbers =
    res |> expand False |> BS.reverse
        |> expand True  |> BS.reverse
        |> expand' alphabet'
  where
    (res, alphabet') = foldl' go (BS.singleton lottery, alphabet) (zip [0 .. ] numbers)

    expand rep str
        | BS.length str < minHashLength
            = let ix = if rep then BS.length str - 3 else 0
                  jx = fromIntegral (BS.index str ix) + hashInt
               in BS.index guards (jx `mod` guardsLength) `BS.cons` str
        | otherwise = str

    expand' ab str
        | BS.length str < minHashLength
            = let ab'  = consistentShuffle ab ab
                  str' = BS.concat [BS.drop halfLength ab', str, BS.take halfLength ab']
               in expand' ab' $ case BS.length str' - minHashLength of
                    n | n > 0
                      -> BS.take minHashLength $ BS.drop (div n 2) str'
                    _ -> str'
        | otherwise = str

    hashInt = numbersHashInt numbers
    lottery = alphabet `BS.index` (hashInt `mod` alphabetLength)
    prefix  = BS.cons lottery salt
    numLast = length numbers - 1
    guardsLength   = BS.length guards
    alphabetLength = BS.length alphabet
    halfLength     = div alphabetLength 2

    go (r, ab) (i, number)
        | number < 0 = error "all numbers must be non-negative"
        | otherwise =
            let shuffled = consistentShuffle ab (BS.append prefix ab)
                last = hash number shuffled
                n = number `mod` (fromIntegral (BS.head last) + i) `mod` BS.length seps
                suffix = if i < numLast
                            then BS.singleton (seps `BS.index` n)
                            else BS.empty
             in (BS.concat [r,last,suffix], shuffled)

-- Exchange elements at positions i and j in a sequence.
exchange :: Int -> Int -> Seq a -> Seq a
exchange i j seq = i <--> j $ j <--> i $ seq
  where
    a <--> b = Seq.update a $ Seq.index seq b

consistentShuffle :: ByteString -> ByteString -> ByteString
consistentShuffle alphabet salt
    | 0 == saltLength = alphabet
    | otherwise = BS.pack $ toList x
  where
    (_,x) = zip3 [len, pred len .. 1] xs ys |> foldl' go (0, toSeq alphabet)

    xs = cycle [0 .. saltLength - 1]
    ys = map (fromIntegral . saltLookup) xs

    saltLookup ix = BS.index salt (ix `mod` saltLength)
    saltLength = BS.length salt

    toSeq = BS.foldl' (Seq.|>) Seq.empty
    len = BS.length alphabet - 1

    go (p, ab) (i, v, ch) =
        let shuffled = exchange i j ab
            p' = p + ch
            j  = mod (ch + v + p') i
         in (p', shuffled)

unhash :: ByteString -> ByteString -> Int
unhash input alphabet = BS.foldl' go 0 input
  where
    go carry item =
        let Just index = BS.elemIndex item alphabet
         in carry * alphabetLength + index
    alphabetLength = BS.length alphabet

hash :: Int -> ByteString -> ByteString
hash input alphabet
    | 0 == input = BS.take 1 alphabet
    | otherwise = BS.reverse $ BS.unfoldr go input
  where
    len = BS.length alphabet
    go 0 = Nothing
    go i = Just (alphabet `BS.index` (i `mod` len), div i len)

-- | Encode a number using the provided salt.
--
--   This convenience function creates a context with the default alphabet.
--   If the same context is used repeatedly, use 'encode' with one of the
--   constructors instead.
encodeUsingSalt :: ByteString     -- ^ Salt
                -> Int            -- ^ Number
                -> ByteString
encodeUsingSalt = encode . hashidsSimple

-- | Encode a list of numbers using the provided salt.
--
--   This function wrapper creates a context with the default alphabet.
--   If the same context is used repeatedly, use 'encodeList' with one of the
--   constructors instead.
encodeListUsingSalt :: ByteString -- ^ Salt
                    -> [Int]      -- ^ Numbers
                    -> ByteString
encodeListUsingSalt = encodeList . hashidsSimple

-- | Decode a hash using the provided salt.
--
--   This convenience function creates a context with the default alphabet.
--   If the same context is used repeatedly, use 'decode' with one of the
--   constructors instead.
decodeUsingSalt :: ByteString     -- ^ Salt
                -> ByteString     -- ^ Hash
                -> [Int]
decodeUsingSalt = decode . hashidsSimple

-- | Shortcut for 'encodeHex'.
encodeHexUsingSalt :: ByteString  -- ^ Salt
                   -> String      -- ^ Hexadecimal number represented as a string
                   -> ByteString
encodeHexUsingSalt = encodeHex . hashidsSimple

-- | Shortcut for 'decodeHex'.
decodeHexUsingSalt :: ByteString  -- ^ Salt
                   -> ByteString  -- ^ Hash
                   -> String
decodeHexUsingSalt = decodeHex . hashidsSimple

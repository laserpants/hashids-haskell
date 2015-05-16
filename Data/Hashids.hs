{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types      #-}

-- | This is a Haskell port of the Hashids library by Ivan Akimov.
-- This is /not/ a cryptographic hashing algorithm. Hashids is typically
-- used to encode numbers to a format suitable for appearance in places 
-- like urls.
--
-- See the official Hashids home page: <http://hashids.org>
-- 
-- Hashids is a small open-source library that generates short, unique, 
-- non-sequential ids from numbers. It converts numbers like 347 into 
-- strings like @yr8@, or a list of numbers like [27, 986] into @3kTMd@.
-- You can also decode those ids back. This is useful in bundling several 
-- parameters into one or simply using them as short UIDs.

module Data.Hashids 
    ( HashidsContext
      -- * How to use
      
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

import Data.Char                ( ord )
import Data.Foldable            ( toList )
import Data.List                ( (\\), elemIndex, foldl', unfoldr, nub, intersect )
import Data.List.Split          ( split, oneOf, dropDelims, chunksOf )
import Data.Maybe               ( fromMaybe )
import Data.Sequence            ( Seq )
import Numeric                  ( showHex, readHex )

import qualified Data.Sequence  as Seq

-- $encoding
--
-- Unless you require a minimum length for the generated hash, create a
-- context using 'hashidsSimple' and then call 'encode' and 'decode' with 
-- this object.
-- 
-- > import Data.Hashids
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
-- > import Data.Hashids
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

-- Exchange elements at positions i and j in a sequence.
exchange :: Int -> Int -> Seq a -> Seq a
exchange i j seq = i <--> j $ j <--> i $ seq
  where
    a <--> b = Seq.update a $ Seq.index seq b

{-# INLINE splitOn #-}
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delims = split (dropDelims $ oneOf delims) 

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) a f = f a

{-# INLINE lookupMod #-}
lookupMod :: Integral n => Int -> [a] -> n -> a
lookupMod modulo xs x = xs !! fromIntegral (x `mod` fromIntegral modulo)

data Alphabet = Alphabet
    { alphabetGlyphs :: String
    , alphabetLength :: Int
    , alphabetLookup :: Integral b => b -> Char
    , alphabetIndex  :: Char -> Int
    }

mkAlphabet :: String -> Alphabet 
mkAlphabet str = Alphabet str len (lookupMod len str) indexOf
  where
    indexOf ch = fromMaybe (error "Not in list!") (elemIndex ch str)
    len = length str

data Salt = Salt
    { saltLookup :: Integral b => b -> Char
    , saltLength :: Int }

mkSalt salt = Salt (lookupMod len salt) len
  where
    len = length salt

-- | Opaque data type that encapsulates various internals required for encoding 
-- and decoding.
data HashidsContext = Context
    { guards        :: !String
    , seps          :: !String
    , salt          :: !String
    , minHashLength :: !Int
    , alphabet      :: !Alphabet 
    } 

-- | Create a context object using the given salt, a minimum hash length, and 
--   a custom alphabet. If you only need to supply the salt, or the first two 
--   arguments, use 'hashidsSimple' or 'hashidsMinimum' instead.
--
--   Changing the alphabet is useful if you want to make your hashes unique,
--   i.e., create hashes different from those generated by other applications 
--   relying on the same algorithm.
createHashidsContext :: String   -- ^ Salt
                     -> Int      -- ^ Minimum required hash length
                     -> String   -- ^ Alphabet
                     -> HashidsContext
createHashidsContext salt minHashLen alphabet 
    | length uniqueAlphabet < minAlphabetLength
        = error $ "alphabet must contain at least " ++ show minAlphabetLength ++ " unique characters"
    | ' ' `elem` uniqueAlphabet 
        = error "alphabet cannot contain spaces"
    | null seps'' || fromIntegral (length alphabet') / fromIntegral (length seps'') > sepDiv
        = case sepsLength - length seps'' of
            diff | diff > 0 
                -> res (drop diff alphabet') (seps'' ++ take diff alphabet')
            _   -> res alphabet' (take sepsLength seps'')
    | otherwise = res alphabet' seps''
  where

    res ab _seps = 
        let shuffled = consistentShuffleS ab salt
            guardCount = ceiling (fromIntegral (length shuffled) / guardDiv)
            context = Context
                { guards        = take guardCount _seps
                , seps          = drop guardCount _seps
                , salt          = salt
                , minHashLength = minHashLen
                , alphabet      = mkAlphabet shuffled }

         in if length shuffled < 3
                then context
                else context{ guards   = take guardCount shuffled
                            , seps     = _seps
                            , alphabet = mkAlphabet $ drop guardCount shuffled }

    seps'  = uniqueAlphabet `intersect` seps
    seps'' = consistentShuffleS seps' salt

    sepsLength = 
        case ceiling (fromIntegral (length alphabet') / sepDiv) of
          1 -> 2
          n -> n

    uniqueAlphabet    = nub alphabet
    alphabet'         = uniqueAlphabet \\ seps
    minAlphabetLength = 16
    sepDiv            = 3.5
    guardDiv          = 12
    seps              = "cfhistuCFHISTU"

defaultAlphabet :: String
defaultAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ "1234567890"

-- | Create a context object using the default alphabet and the provided salt,
--   without any minimum required length.
hashidsSimple :: String  -- ^ Salt
              -> HashidsContext
hashidsSimple salt = createHashidsContext salt 0 defaultAlphabet

-- | Create a context object using the default alphabet and the provided salt.
--   The generated hashes will have a minimum length as specified by the second 
--   argument.
hashidsMinimum :: String          -- ^ Salt
               -> Int             -- ^ Minimum required hash length
               -> HashidsContext
hashidsMinimum salt minimum = createHashidsContext salt minimum defaultAlphabet

-- | Encode a hexadecimal number.
--
-- /Example use:/
--
-- > encodeHex context "ff83"       -- "yzgwD"
--
encodeHex :: HashidsContext     -- ^ A Hashids context object
          -> String             -- ^ Hexadecimal number represented as a string
          -> String
encodeHex context str 
    | not (all hexChar str) = ""
    | otherwise = encodeList context $ map go $ chunksOf 12 str
  where
    go str = let [(a,_)] = readHex ('1':str) in a
    hexChar c = c `elem` "0123456789abcdef"

-- | Decode a hash generated with 'encodeHex'.
--
-- /Example use:/
--
-- > decodeHex context "yzgwD"      -- "ff83"
--
decodeHex :: HashidsContext     -- ^ A Hashids context object
          -> String             -- ^ Hash
          -> String
decodeHex context hash = 
    concatMap (drop 1 . flip showHex "") numbers
  where
    numbers = decode context hash

numbersHashInt :: Integral a => [a] -> a
numbersHashInt xs = foldr ((+) . uncurry mod) 0 $ zip xs [100 .. ]

-- | Encode a single number.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = encode context 5        -- == "rD"
--
encode :: Integral n 
       => HashidsContext        -- ^ A Hashids context object
       -> n                     -- ^ Number to encode
       -> String
encode context n = encodeList context [n]

-- | Encode a list of numbers.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = encodeList context [2, 3, 5, 7, 11]          -- == "EOurh6cbTD"
--
encodeList :: Integral n 
           => HashidsContext    -- ^ A Hashids context object
           -> [n]               -- ^ List of numbers
           -> String
encodeList Context{ alphabet = alphabet@Alphabet{ alphabetLength = len }, .. } numbers = 
     res |> expand (++) 0 
         |> expand (flip (++)) 2 
         |> expand' alphabet'
  where
    (res, alphabet') = foldl' go ([lottery], alphabet) (zip [0 .. ] numbers)

    expand coalesce index str = coalesce 
        [ lookupMod guardsLength guards $ ord (str !! index) + fromIntegral hashInt
            | length str < minHashLength ] str

    expand' ab str
        | length str >= minHashLength = str
        | otherwise =
            let ab'   = consistentShuffle_ ab len (alphabetGlyphs ab)
                chars = alphabetGlyphs ab'
                str'  = concat [drop halfLength chars, str, take halfLength chars]
             in expand' ab' $ case length str' - minHashLength of
                  n | n > 0 
                    -> take minHashLength $ drop (div n 2) str'
                  _ -> str'

    hashInt = numbersHashInt numbers
    lottery = alphabetLookup alphabet hashInt 
    prefix  = lottery : salt

    go (r, ab@Alphabet{..}) (i, number)  
        | number < 0 = error "all numbers must be non-negative"
        | otherwise  = 
            let ab'  = consistentShuffle_ ab alphabetLength (prefix ++ alphabetGlyphs)
                last = hash number ab' 
                n = (fromIntegral number `mod` (ord (head last) + i)) `mod` sepsLength
                suffix = [seps !! n | i < lastNumber]
             in (r ++ last ++ suffix, ab')

    sepsLength   = length seps
    guardsLength = length guards
    lastNumber   = length numbers - 1 
    halfLength   = div len 2

-- | Decode a hash.
--
-- /Example use:/
--
-- > let context = hashidsSimple "this is my salt"
-- >     hash = decode context "rD"        -- == [5]
--
decode :: Integral n 
       => HashidsContext     -- ^ A Hashids context object
       -> String             -- ^ Hash
       -> [n]
decode _ "" = []
decode ctx@Context{..} hash 
    | "" == str = []
    | encodeList ctx res /= hash = []
    | otherwise = res
  where
    res = splitOn seps tail
            |> foldl' go ([], alphabet) 
            |> fst
            |> reverse

    hashArray = splitOn guards hash
    (Alphabet glyphs len _ _) = alphabet

    str@(lottery:tail) = 
         hashArray !! case length hashArray of
            0 -> error "Internal error."
            2 -> 1
            3 -> 1
            _ -> 0

    prefix = lottery : salt
    go (xs, ab) ssh = 
        let buffer = prefix ++ alphabetGlyphs ab
            ab'    = consistentShuffle_ ab len buffer
         in (unhash ssh ab':xs, ab')

consistentShuffleS :: String -> String -> String
consistentShuffleS alphabet salt = alphabetGlyphs $ consistentShuffle (mkAlphabet alphabet) (mkSalt salt)

consistentShuffle_ :: Alphabet -> Int -> String -> Alphabet 
consistentShuffle_ alphabet len = consistentShuffle alphabet . mkSalt . take len 

consistentShuffle :: Alphabet -> Salt -> Alphabet 
consistentShuffle alphabet@Alphabet{..} Salt{..} 
    | 0 == saltLength = alphabet
    | otherwise      = mkAlphabet $ toList x 
  where
    (_,x) = zip3 [len, pred len .. 1] xs ys |> foldl' go (0, Seq.fromList alphabetGlyphs) 

    xs = cycle [0 .. saltLength - 1]
    ys = map (ord . saltLookup) xs

    go (p, ab) (i, v, ch) =  
        let shuffled = exchange i j ab
            p' = p + ch
            j  = mod (ch + v + p') i
         in (p', shuffled)

    len = alphabetLength - 1

unhash :: Integral n => String -> Alphabet -> n
unhash input Alphabet{..} = foldl' go 0 $ zip [len, len - 1 .. ] input 
  where
    go number (i, char) = 
        number + fromIntegral (alphabetIndex char * alphabetLength ^ i)
    len = pred $ length input

hash :: Integral n => n -> Alphabet -> String
hash input Alphabet{..}  
    | 0 == input = [alphabetLookup input]
    | otherwise = go (input, [])
  where
    len = fromIntegral alphabetLength
    go (0, xs) = xs
    go (x, xs) = go (div x len, alphabetLookup x:xs)

-- | Encode a number using the provided salt. 
--
--   This convenience function creates a context with the default alphabet. 
--   If the same context is used repeatedly, use 'encode' with one of the 
--   constructors instead.
encodeUsingSalt :: Integral n 
                => String         -- ^ Salt
                -> n              -- ^ Number
                -> String
encodeUsingSalt = encode . hashidsSimple

-- | Encode a list of numbers using the provided salt. 
--
--   This function wrapper creates a context with the default alphabet. 
--   If the same context is used repeatedly, use 'encodeList' with one of the
--   constructors instead.
encodeListUsingSalt :: Integral n 
                    => String     -- ^ Salt
                    -> [n]        -- ^ Numbers
                    -> String
encodeListUsingSalt = encodeList . hashidsSimple

-- | Decode a hash using the provided salt. 
--
--   This convenience function creates a context with the default alphabet. 
--   If the same context is used repeatedly, use 'decode' with one of the 
--   constructors instead.
decodeUsingSalt :: Integral n 
                => String         -- ^ Salt
                -> String         -- ^ Hash
                -> [n]
decodeUsingSalt = decode . hashidsSimple

-- | Shortcut for 'encodeHex'.
encodeHexUsingSalt :: String      -- ^ Salt
                   -> String      -- ^ Hexadecimal number represented as a string
                   -> String
encodeHexUsingSalt = encodeHex . hashidsSimple

-- | Shortcut for 'decodeHex'.
decodeHexUsingSalt :: String      -- ^ Salt
                   -> String      -- ^ Hash
                   -> String
decodeHexUsingSalt = decodeHex . hashidsSimple


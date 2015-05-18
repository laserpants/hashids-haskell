# hashids-haskell

Haskell port of the Hashids library. http://hashids.org

* Docs: http://johanneshilden.github.io/hashids-haskell/dist/doc/html/hashids/Web-Hashids.html
* Hackage: http://hackage.haskell.org/package/hashids

Install: 
```
cabal install hashids
```

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Hashids

main :: IO ()
main = do
    let hashids = hashidsSimple "this is my salt"
        id = encodeList hashids [1, 2, 3]
        numbers = decode hashids id

    print (id, numbers)
```

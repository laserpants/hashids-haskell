# hashids-haskell  [![Build Status](https://img.shields.io/travis/johanneshilden/hashids-haskell/master.svg?style=flat)](https://travis-ci.org/johanneshilden/hashids-haskell)

Haskell port of the Hashids library. http://hashids.org

* Hackage: http://hackage.haskell.org/package/hashids
* Docs: http://hackage.haskell.org/package/hashids-1.0.2.1/docs/Web-Hashids.html

##### Install: 

```
cabal install hashids
```

##### Hello, World:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Hashids

main :: IO ()
main = do
    let hashids = hashidsSimple "this is my salt"
        ids = encodeList hashids [1, 2, 3]
        numbers = decode hashids ids

    print (ids, numbers)
```

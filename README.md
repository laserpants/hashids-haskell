# ![hashids-haskell](http://hashids.org/public/img/hashids-logo-normal.png "Hashids") [![Build Status](https://img.shields.io/travis/laserpants/hashids-haskell/master.svg?style=flat)](https://travis-ci.org/laserpants/hashids-haskell)

Haskell port of the Hashids library. http://hashids.org

* Hackage: http://hackage.haskell.org/package/hashids
* Stackage: https://www.stackage.org/package/hashids
* Docs: http://hackage.haskell.org/package/hashids/docs/Web-Hashids.html

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

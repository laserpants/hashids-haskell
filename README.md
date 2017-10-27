# ![hashids-haskell](http://hashids.org/public/img/hashids-logo-normal.png "Hashids") [![Build Status](https://img.shields.io/travis/laserpants/hashids-haskell/master.svg?style=flat)](https://travis-ci.org/laserpants/hashids-haskell) [![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Language](https://img.shields.io/badge/language-Haskell-orange.svg)](https://www.haskell.org/) [![Hackage](https://img.shields.io/hackage/v/hashids.svg)](http://hackage.haskell.org/package/hashids)  [![Version](https://img.shields.io/badge/api.version-1.0.svg?colorB=ff69b4)](http://hashids.org/)

Haskell port of the Hashids library. http://hashids.org

* Hackage: http://hackage.haskell.org/package/hashids
* Stackage: https://www.stackage.org/package/hashids
* Docs: http://hackage.haskell.org/package/hashids/docs/Web-Hashids.html

##### Install: 

```
cabal install hashids
```

or using [Stack](https://www.haskellstack.org/):

```
stack install hashids
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

#!/bin/sh -x
[ -z "$CABAL" ] && CABAL=cabal

set -e
which cabal

$CABAL clean
$CABAL check

$CABAL install --only-dependencies --enable-tests
$CABAL configure --enable-tests
ghc-pkg list
$CABAL install Cabal-1.18.1.4
runhaskell -- -package --ghc-arg=Cabal-1.18.1.4 src-tools/package-info.hs --package
$CABAL build

$CABAL sdist

dist/build/tests/tests --hide-successes --maximum-generated-tests=10000 --maximum-unsuitable-generated-tests=10000 --jxml=junit-log.xml

nameBase=`runhaskell -- -package --ghc-arg=Cabal-1.18.1.4 src-tools/package-info.hs --package`
$CABAL install dist/$nameBase.tar.gz --enable-tests

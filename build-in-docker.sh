#!/bin/sh

# Run this script with
#
#   sh build-in-docker.sh
#
# To produce a simple bindist in dist-newstyle/bindist

set -ex

if [ "x$DOCKER" = "xYES" ]; then
  cd /build
  cabal update

  cd /build/src
  cabal build --builddir=/build/builddir all

  TARGET=$(cabal run --builddir=/build/builddir -- cabal-plan list-bin --builddir=/build/builddir cabal-plan | tail -n1)
  VERSION=$("$TARGET" --version | awk '{ print $2 }')

  cp "$TARGET" /build/bindist/cabal-plan
  strip /build/bindist/cabal-plan
  xz -c < /build/bindist/cabal-plan > "/build/bindist/cabal-plan-${VERSION}-x86_64-linux.xz"

  ls -lh /build/bindist

else

  mkdir -p dist-newstyle/bindist
  docker run --rm -ti -e DOCKER=YES -v "$(pwd):/build/src:ro" -v "$(pwd)/dist-newstyle/bindist:/build/bindist" phadej/ghc:8.6.5-xenial sh /build/src/build-in-docker.sh
  cd dist-newstyle/bindist
  sha256sum cabal-plan-*.xz > SHA256SUMS
  gpg2 --sign --detach-sig --armor SHA256SUMS

fi

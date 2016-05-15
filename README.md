hs-helm-samples
===============

Example/small/test games written with the [Haskell](http://www.haskell.org/) [Helm](http://helm-engine.org) reactive game engine.


Installation (OpenBSD)
============

Libraries

    pkg_add -iv sdl2
    pkg_add -iv cairo
    pkg_add -iv pango


Haskell

    git clone https://github.com/z0w0/helm.git
    cd helm/
    cabal sandbox init
    cabal install gtk2hs-buildtools
    cabal install

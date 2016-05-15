hs-helm-samples
===============

Example/small/test games written with the [Haskell](http://www.haskell.org/) [Helm](http://helm-engine.org) reactive game engine.


Installation (Mac OS X with MacPorts)
=======================

Libraries

    sudo port install cairo-devel
    sudo port install pango-devel
    sudo port install libsdl2
    
Haskell

    git clone https://github.com/z0w0/helm.git
    cd helm/
    stack install

Installation (OpenBSD)
======================

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

BTW: On OpenBSD (5.9) I get the following error:

    SDLCallFailed {sdlExceptionCaller = "SDL.Init.init",
                   sdlFunction = "SDL_Init",
                   sdlExceptionError = "No available video device"}

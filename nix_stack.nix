{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  inherit ghc;
  buildInputs = with pkgs; [
    alsaLib
    git git-lfs
    gmp
    haskellPackages.yeganesh
    openssl
    wirelesstools
    xorg.libX11 xorg.libXext xorg.libXft xorg.libXpm xorg.libXrandr xorg.libXScrnSaver
    xorg.libXdmcp
    cairo glib expat pcre2 pango util-linux libselinux libsepol fribidi libthai
    libdatrie
    xscreensaver
    zlib
  ];
  LANG = "en_US.UTF-8";
}

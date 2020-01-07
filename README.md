
## Building

After cloning, initialize submodules:

```
git submodule init
git submodule update
```


Link `$HOME/.xmonad` to this repo:

```
ln -s ~/workspace/dotxmonad .xmonad
```

### Ubuntu

Install some deps:

- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- `dmenu`
- `libiw-dev`
- `libxpm-dev`
- `libasound2-dev`
- `yeganesh` (`stack install yeganesh`)

In this repo's directory, use the right build script:
`ln -s build_stack.sh build`.

Now build all the dependencies:

```
stack build
cd xmobar-git && stack install
```

And finally:

```
xmonad --recompile && xmonad --restart
```

### Nix

In this repo's directory, use the right build script:
`ln -s build_stack_nix.sh build`.

Run the following commands to get a working shell for the build:


Now build all the dependencies:

```
stack --nix build
cd xmobar-git && stack install
```

#### Untested:

```
nix-shell nix_stack.nix
nix-shell -p haskellPackages.xmonad
unset STACK_IN_NIX_SHELL
```

And finally:

```
xmonad --recompile && xmonad --restart
```


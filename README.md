
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

### General notes
1. When running any variation of `stack build` below, if a failure occurs,
sometimes it helps to keep running the command over until the failure
becomes idempotent; often some more dependencies will install in subsequent
iterations, and the error will become more clear once idempotent (e.g. 
narrowing down a missing system dependency).

### Ubuntu

Install some deps:

- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- apt deps (potentially incomplete): `sudo apt install dmenu libiw-dev libxpm-dev libasound2-dev libxft-dev libxinerama-dev libxrandr-dev libxss-devxscreensaver`
- `yeganesh` (`stack install yeganesh`)

In this repo's directory, use the right build script:
`ln -s build_stack.sh build`.

Now build all the dependencies:

```
stack build
stack install
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


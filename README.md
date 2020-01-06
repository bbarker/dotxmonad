
## Building

### Ubuntu

Install some deps:

- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- `libiw-dev`
- `libxpm-dev`

Use the right build script: `ln -s build_stack.sh build`.

### Nix

Use the right build script: `ln -s build_stack_nix.sh build`.

Run the following commands to get a working shell for the build:

```
nix-shell nix_stack.nix
nix-shell -p haskellPackages.xmonad
unset STACK_IN_NIX_SHELL
```

And finally:

```
xmonad --recompile && xmonad --restart
```


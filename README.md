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
becomes goes away; often some more dependencies will install in subsequent
iterations, and the error will become more clear once idempotent (e.g. 
narrowing down a missing system dependency).

### Ubuntu

`LANG` should be set as follows:

```
export LANG=C.UTF-8
```

Install some deps:

- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- apt deps (potentially incomplete): `sudo apt install dmenu xscreensaver x11proto-scrnsaver-dev libiw-dev libxpm-dev libasound2-dev libxft-dev libxinerama-dev libxrandr-dev libgmp-dev libxss-dev libpango1.0-dev dbus-x11`
- `yeganesh` (`stack install yeganesh`)

In this repo's directory, use the right build script:
`ln -s build_stack.sh build`.

You *may* need to get a shell without a Nix environment to proceed (though in
a more recent Nix install this wasn't necessary), assuming you
have Nix installed for this user. We suggest adding a conditional around
the appropriate sections of `~/.bashrc` and/or `~/.profile`, e.g.:

```bash
if [ -z ${NO_NIX} ]; then
    source /home/brandon/.nix-profile/etc/profile.d/nix.sh
fi
```

Now you can get the environment to build in:

```
env -i HOME="$HOME" USER="$USER" NO_NIX=1 bash -l
```

Now build all the dependencies:

```
stack build
stack install
```


Some applications may require D-BUS environment variables set, so add
this to your `.bashrc`:

```
export $(dbus-launch)
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

## All Systems - Finishing Touches


```
ln -s $(pwd)xmobarrc ~/.xmobarrc
```

And finally:

```
xmonad --recompile && xmonad --test && xmonad --restart
```

Note that `--test` doesn't really do anything, but it doesn't restart
the existing xmonad process so it should be a good way to test some
runtime issues (it will just return a 'user error').




## Updating xmonad, xmobar, etc.

```
cd xmobar-git
git remote add upstream https://codeberg.org/xmobar/xmobar.git # only once
git checkout master && git pull upstream master && git push
cd ..

cd xmonad-git
git remote add upstream https://github.com/xmonad/xmonad.git
git checkout master && git pull upstream master && git push
cd ..

cd xmonad-contrib-git
git remote add upstream https://github.com/xmonad/xmonad-contrib.git
git checkout master && git pull upstream master && git push
cd ..

git submodule foreach 'git pull origin $(git rev-parse --abbrev-ref HEAD)'
git update-index
```

After updating and testing, don't forget to commit and push!

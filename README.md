![GNU Emacs logo](https://www.gnu.org/software/emacs/images/emacs.png "GNU Emacs")

# GNU Emacs #

-------------------------------------------------------------------------------

**GNU Emacs in a snap** - the extensible, customizable, self-documenting real-time display editor

[![GNU Emacs](https://snapcraft.io/emacs/badge.svg)](https://snapcraft.io/emacs)

## Installation ##

[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-black.svg)](https://snapcraft.io/emacs)

``` shell
sudo snap install emacs --classic
```

([Don't have snapd installed?](https://snapcraft.io/docs/core/install))

## Build it yourself ##

```shell
# clone this repo
git clone https://github.com/alexmurray/emacs-snap
cd emacs-snap

# install snapcraft and multipass tooling needed to build the snap in a reproducible way
sudo snap install snapcraft --classic
sudo snap install multipass --beta --classic

# build the snap
snapcraft

# install the snap (--dangerous signals this is not signed and hence not trusted)
sudo snap install ./emacs*.snap --dangerous
```

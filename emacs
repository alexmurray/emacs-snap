#!/bin/bash

# Launch emacs with the most appropriate toolkit (currently gtk (for real x11)
# or wayland)

# The environment variable EMACS_TOOLKIT can be used to specify the preferred toolkit.
# The value should be one of: gtk or wayland

# If the environment variable is not set, the script will try to detect the
# toolkit to use by checking if the XDG_SESSION_TYPE environment variable is
# set to wayland or x11.

if [ -n "${XDG_SESSION_TYPE}" ]; then
  if [ "${XDG_SESSION_TYPE}" = "wayland" ]; then
    : "${EMACS_TOOLKIT:=wayland}"
  else
    : "${EMACS_TOOLKIT:=gtk}"
  fi
else
  : "${EMACS_TOOLKIT:=gtk}"
fi

case "${EMACS_TOOLKIT}" in
  gtk|wayland)
    ;;
  *)
    echo "Invalid value for EMACS_TOOLKIT: ${EMACS_TOOLKIT} - must be either gtk or wayland"
    exit 1
    ;;
esac

exec "$SNAP/usr/bin/emacs-${EMACS_TOOLKIT}" "$@"


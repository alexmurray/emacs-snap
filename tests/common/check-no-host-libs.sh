#!/bin/bash
# Check that no process belonging to the emacs snap has mapped in a library
# from the host.
#
# This started out driving classic-snap-analyzer (see
# https://snapcraft.io/classic-snap-analyzer) but that tool (0.3.1) has two
# problems that make it unusable as-is:
#
# 1. it only recognizes the base snap by the literal name "core" (see the
#    exclusion regex in
#    /snap/classic-snap-analyzer/current/bin/classic-snap-analyzer), so on
#    core26 (and any other numbered base) it misreports every library the
#    process maps in from the base snap as coming from the host.
#
# 2. it matches every process whose environ contains SNAP_NAME=emacs, which
#    also catches genuine children of emacs that intentionally exec a
#    *host* helper (e.g. gdk-pixbuf's glycin loaders, sandboxed via bwrap,
#    for SVG rendering) - those legitimately run host code with host
#    libraries and are not something we ship or can bundle our way out of,
#    so they shouldn't count as a leak from our own binaries.
#
# so instead we re-implement its core /proc scan here, scoped to processes
# whose own executable is actually one of ours.
#
# Usage: check-no-host-libs.sh [label]
#
# Run this locally (after starting emacs, e.g. `emacs -nw --batch --eval
# '(sleep-for 30)' &`) to check without needing a full spread run.

set -u

label="${1:-check-no-host-libs}"
errfile=$(mktemp)
trap 'rm -f "$errfile" "$errfile.real"' EXIT

for p in /proc/[0-9]*; do
  pid=${p#/proc/}
  # [ -r ] is only a fast-path skip: it can pass (same UID, mode bits allow
  # it) while the actual read is still denied by e.g. Yama ptrace_scope, so
  # the real open() attempt below must have its own stderr suppressed too -
  # piping through cat rather than a direct `<` redirect ensures the EACCES
  # happens inside a redirected subprocess instead of failing the shell's own
  # redirection setup (which a trailing 2>/dev/null can't catch).
  [ -r "$p/environ" ] || continue
  cat "$p/environ" 2>/dev/null | tr '\0' '\n' | grep -q '^SNAP_NAME=emacs$' || continue
  case "$(readlink "$p/exe" 2>/dev/null)" in
    /snap/emacs/*) ;;
    *) continue ;;
  esac
  # require an actual shared-object filename (.so or .so.N.N...), not just a
  # '/lib' substring anywhere in the path - that loose match also fires on
  # unrelated files that happen to contain it, e.g. fonts under a
  # "liberation" directory, gettext catalogs named after the "libc" domain
  # (.../LC_MESSAGES/libc.mo), or dictionary data under /var/lib/aspell/.
  awk '{ if ($6 != "") print $6 }' "$p/maps" 2>/dev/null | grep -E '\.so(\.[0-9]+)*$'
done | grep -v -E "^/snap/(core[0-9]*|emacs)/" | sort -u >> "$errfile"

# libcanberra dlopens its sound backend plugins via a hardcoded absolute
# host path baked into libcanberra0 at package-build time (see
# /usr/lib/*/libcanberra.so.0 - it sprintfs
# "/usr/lib/<arch>/libcanberra-0.30/libcanberra-%s") which RPATH/
# LD_LIBRARY_PATH cannot redirect, so bundling our own copy can never help -
# this is a permanent, unavoidable exception, kept so the audio bell still
# works on hosts that have pulseaudio.
grep -v -E "/libcanberra-0\.30/libcanberra-pulse\.so$" "$errfile" > "$errfile.real" || true
mv "$errfile.real" "$errfile"

if [ -s "$errfile" ]; then
  echo "$label: libraries unexpectedly loaded from the host:"
  cat "$errfile"
  exit 1
fi

exit 0

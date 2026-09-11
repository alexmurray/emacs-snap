# Required to exist by zyga/image-garden-action (github.com/zyga/image-
# garden-action) - its "Restore mtime of .image-garden.mk" step
# unconditionally runs `git restore-mtime .image-garden.mk` and its VM-image
# cache key is `hashFiles('.image-garden.mk')`, so a missing file breaks the
# whole job even though image-garden itself only needs this file for
# project-specific customization (see image-garden(1)), which we don't need:
# all our systems (spread.yaml's backends.garden.systems) are stock
# image-garden cloud images, and package installation happens in
# spread.yaml's own prepare: scripts rather than cloud-init templates.

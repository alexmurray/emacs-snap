/**
 * This is a helper program to setup the environment for the emacs snap to run
 * correctly.
 * Copyright 2025 Alex Murray <murray.alex@gmail.com>
 * License: GPL-3.0+
 */

#define _GNU_SOURCE // for asprintf

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "variant.h"

#ifndef VARIANT
#error "VARIANT should have been defined in variant.h"
#endif

static int debug = 0;

#define dbg(fmt, ...)                                                          \
  do {                                                                         \
    if (debug) {                                                               \
      fprintf(stderr, "%s:%d: " fmt, __FILE__, __LINE__, ##__VA_ARGS__);       \
    }                                                                          \
  } while (0)

// check if str ends with suffix
static int str_ends_with(const char *str, const char *suffix) {
  size_t str_len = strlen(str);
  size_t suffix_len = strlen(suffix);
  if (suffix_len > str_len) {
    return 0;
  }
  return strncmp(str + str_len - suffix_len, suffix, suffix_len) == 0;
}

static int nftw_unlink(const char *fpath, const struct stat *sb, int typeflag,
                       struct FTW *ftwbuf) {
  int res;

  if (ftwbuf->level == 0) {
    // skip the root directory
    dbg("Skipping root directory %s\n", fpath);
    return 0;
  }

  // unlink files
  switch (typeflag) {
  case FTW_F:
  case FTW_SL:
  case FTW_SLN:
    // regular file or symbolic link
    dbg("Unlinking %s\n", fpath);
    res = unlink(fpath);
    if (res < 0) {
      fprintf(stderr, "Failed to unlink %s: %s\n", fpath, strerror(errno));
    }
    break;

  case FTW_D:
  case FTW_DP:
    // directory or directory with children processed
    dbg("Removing directory %s\n", fpath);
    res = rmdir(fpath);
    if (res < 0) {
      fprintf(stderr, "Failed to rmdir %s: %s\n", fpath, strerror(errno));
    }
    break;

  case FTW_DNR:
  case FTW_NS:
    // unreadable directory or unstatable file
    // don't do anything here
    dbg("Skipping %s - unreadable or unstatable\n", fpath);
    res = 0;
    break;
  }
  return res; // continue nftw traversal unless we have hit an error
}

// NOTE: in general we don't bother to free any of the dynamically allocated
// memory since this is not a long-lived process so we don't care about memory
// leaks as it will all get cleaned up when the process exits
int main(int argc, char *argv[]) {
  int res;
  int overwrite;
  struct stat st;
  const char *snap;
  const char *snap_arch;
  const char *snap_user_common;
  char *arch = NULL;
  char *variant_path;
  char *gdk_cache_dir = NULL;
  char *gio_module_dir = NULL;
  char *gdk_pixbuf_module_file = NULL;
  char *gdk_pixbuf_moduledir = NULL;
  char *gdk_pixbuf_query_loaders = NULL;
  char *fontconfig_cache_dir = NULL;
  char *fontconfig_file = NULL;
  char *gtk_im_module_dir = NULL;
  char *gtk_im_module_file = NULL;
  char *gtk_query_immodules = NULL;
  const char *path = NULL;

  if (getenv("SNAP_SETUP_ENV_DEBUG") != NULL) {
    debug = 1;
  }

  dbg("Running setup-env for emacs snap variant %s\n", VARIANT);

  snap = getenv("SNAP");
  if (snap == NULL) {
    fprintf(stderr, "SNAP is not set\n");
    exit(1);
  }
  dbg("Using SNAP %s\n", snap);

  snap_arch = getenv("SNAP_ARCH");
  if (snap_arch == NULL) {
    fprintf(stderr, "SNAP_ARCH is not set\n");
    exit(1);
  }
  dbg("Using SNAP_ARCH %s\n", snap_arch);

  if (strcmp(snap_arch, "amd64") == 0) {
    asprintf(&arch, "x86_64-linux-gnu");
  } else if (strcmp(snap_arch, "armhf") == 0) {
    asprintf(&arch, "arm-linux-gnueabihf");
  } else if (strcmp(snap_arch, "arm64") == 0) {
    asprintf(&arch, "aarch64-linux-gnu");
  } else {
    asprintf(&arch, "%s-linux-gnu", snap_arch);
  }

  dbg("Using architecture %s\n", arch);

  snap_user_common = getenv("SNAP_USER_COMMON");
  if (snap_user_common == NULL) {
    fprintf(stderr, "SNAP_USER_COMMON is not set\n");
    exit(1);
  }

  dbg("Using SNAP_USER_COMMON %s\n", snap_user_common);

  asprintf(&variant_path, "%s/.emacs-variant", snap_user_common);
  dbg("Using variant path %s\n", variant_path);
  // read/write variant so we can check if we have changed variant and hence
  // need to re-run everything
  {
    FILE *fp = fopen(variant_path, "r");
    if (fp != NULL) {
      // if we can open the variant file then we check if it matches what we
      // expect
      dbg("Reading variant from %s\n", variant_path);
      char line[256];
      if (fgets(line, sizeof(line), fp) != NULL) {
        // remove trailing newline
        if (line[strlen(line) - 1] == '\n') {
          line[strlen(line) - 1] = '\0';
        }
        // check if the variant matches what we expect
        if (strcmp(line, VARIANT) != 0) {
          dbg("Variant '%s' does not match expected '%s'\n", line, VARIANT);
          overwrite = 1;
        } else {
          dbg("Variant '%s' matches expected '%s'\n", line, VARIANT);
          overwrite = 0;
        }
      } else {
        dbg("Failed to read variant from %s: %s\n", variant_path,
            strerror(errno));
        overwrite = 1;
      }
      fclose(fp);
    } else {
      // if we can't open the variant file then we assume we are running the
      // first time and hence need to write the variant
      dbg("Writing variant '%s' to %s\n", VARIANT, variant_path);
      fp = fopen(variant_path, "w");
      if (fp == NULL) {
        fprintf(stderr, "Failed to open %s for writing: %s\n", variant_path,
                strerror(errno));
        exit(1);
      }
      fprintf(fp, "%s\n", VARIANT);
      fclose(fp);
    }
  }

  overwrite = overwrite | !!getenv("SNAP_SETUP_ENV_OVERWRITE");

  if (overwrite) {
    dbg("Removing existing environment setup in %s\n", snap_user_common);
    // remove and recreate SNAP_USER_COMMON entirely including all its contents
    if (nftw(snap_user_common, nftw_unlink, 10, FTW_DEPTH | FTW_PHYS) != 0) {
      fprintf(stderr, "Failed to remove contents of %s: %s\n", snap_user_common,
              strerror(errno));
      exit(1);
    }
    // write the variant file again
    {
      dbg("Writing variant '%s' to %s\n", VARIANT, variant_path);
      FILE *fp = fopen(variant_path, "w");
      if (fp == NULL) {
        fprintf(stderr, "Failed to open %s for writing: %s\n", variant_path,
                strerror(errno));
        exit(1);
      }
      fprintf(fp, "%s\n", VARIANT);
      fclose(fp);
    }
  }

  asprintf(&gdk_cache_dir, "%s/.cache", snap_user_common);
  mkdir(gdk_cache_dir, 0700);

  asprintf(&gio_module_dir, "%s/usr/lib/%s/gio/modules", snap, arch);
  setenv("GIO_MODULE_DIR", gio_module_dir, 1);

  asprintf(&gdk_pixbuf_module_file, "%s/gdk-pixbuf-loaders.cache",
           gdk_cache_dir);
  setenv("GDK_PIXBUF_MODULE_FILE", gdk_pixbuf_module_file, 1);

  asprintf(&gdk_pixbuf_moduledir, "%s/usr/lib/%s/gdk-pixbuf-2.0/2.10.0/loaders",
           snap, arch);
  setenv("GDK_PIXBUF_MODULEDIR", gdk_pixbuf_moduledir, 1);

  asprintf(&gdk_pixbuf_query_loaders,
           "%s/usr/lib/%s/gdk-pixbuf-2.0/gdk-pixbuf-query-loaders", snap, arch);
  res = stat(gdk_pixbuf_query_loaders, &st);

  if (res == 0) {
    dbg("Found gdk-pixbuf-query-loaders at %s\n", gdk_pixbuf_query_loaders);
    // execute gdk_pixbuf_query_loaders and redirect output to
    // gdk_pixbuf_module_file
    pid_t child = fork();
    if (child == -1) {
      fprintf(stderr, "Failed to fork: %s\n", strerror(errno));
      exit(1);
    }
    if (child == 0) {
      // we are the child - redirect our output to gdk_pixbuf_module_file and
      // exec gdk_pixbuf_query_loaders
      int fd = open(gdk_pixbuf_module_file, O_CREAT | O_TRUNC | O_WRONLY, 0644);
      if (fd < 0) {
        fprintf(stderr, "Failed to open %s: %s\n", gdk_pixbuf_module_file,
                strerror(errno));
        exit(1);
      }
      res = dup2(fd, 1);
      if (res < 0) {
        fprintf(stderr, "Failed to dup2: %s\n", strerror(errno));
        exit(1);
      }
      dbg("Executing %s\n", gdk_pixbuf_query_loaders);
      res = execl(gdk_pixbuf_query_loaders, gdk_pixbuf_query_loaders, NULL);
      if (res < 0) {
        fprintf(stderr, "Failed to exec %s: %s\n", gdk_pixbuf_query_loaders,
                strerror(errno));
        exit(1);
      }
    }
    // wait for child to execute
    waitpid(child, NULL, 0);
  }

  asprintf(&fontconfig_cache_dir, "%s/.cache/fontconfig", snap_user_common);
  mkdir(fontconfig_cache_dir, 0700);

  asprintf(&fontconfig_file, "%s/fonts.conf", snap_user_common);
  setenv("FONTCONFIG_FILE", fontconfig_file, 1);

  // always recreate the fontconfig_file to ensure we overwrite any old one that
  // may have come from a different base snap version in a previous release of
  // the emacs snap
  {
    char *source_path;
    int infd, outfd;
    FILE *infile;
    char *line = NULL;
    size_t n;
    ssize_t len;
    regex_t re;
    regmatch_t matches[1];
    char *cachedir_entry;

    res = regcomp(&re, "<cachedir.*/cachedir>", REG_EXTENDED);
    if (res != 0) {
      fprintf(stderr, "Failed to compile regex\n");
      exit(1);
    }
    asprintf(&cachedir_entry, "	<cachedir>%s</cachedir>\n",
             fontconfig_cache_dir);
    asprintf(&source_path, "%s/etc/fonts/fonts.conf", snap);
    infd = open(source_path, O_RDONLY);
    if (infd == -1) {
      fprintf(stderr, "Failed to open source fontconfig file %s: %s\n",
              source_path, strerror(errno));
    }
    outfd =
      open(fontconfig_file, O_CREAT | O_TRUNC | O_WRONLY | O_CLOEXEC, 0644);
    if (outfd == -1) {
      fprintf(stderr, "Failed to open dest fontconfig file %s: %s\n",
              fontconfig_file, strerror(errno));
    }
    res = fstat(infd, &st);
    if (res == -1) {
      fprintf(stderr, "Failed to stat infd: %s\n", strerror(errno));
    }
    infile = fdopen(infd, "r");
    if (infile == NULL) {
      fprintf(stderr, "Failed to open %s for streaming: %s\n", source_path,
              strerror(errno));
    }
    // read infd line by line and write the output to outfd - check along the
    // way for cachedir entries and replace these with our own
    // fontconfig_cache_dir
    while ((len = getline(&line, &n, infile)) != -1) {
      res = regexec(&re, line, 1, matches, 0);
      if (res == 0) {
        write(outfd, cachedir_entry, strlen(cachedir_entry));
      } else {
        write(outfd, line, strlen(line));
      }
      free(line);
      line = NULL;
    }
    fclose(infile); // fclose will close infd
    close(outfd);
    free(source_path);
    free(cachedir_entry);
    regfree(&re);
  }

  // immodule cache
  asprintf(&gtk_im_module_dir, "%s/immodules", gdk_cache_dir);
  mkdir(gtk_im_module_dir, 0700);

  asprintf(&gtk_im_module_file, "%s/immodules.cache", gtk_im_module_dir);
  asprintf(&gtk_query_immodules,
           "%s/usr/lib/%s/libgtk-3-0/gtk-query-immodules-3.0", snap, arch);
  res = stat(gtk_query_immodules, &st);
  if (res == 0) {
    // execute gtk_query_immodules over the list of immodules and redirect
    // output to gtk_im_module_file
    // first enumerate the list of immmodules
    char **args = NULL;
    int nargs = 0;
    char *gtk_immodules_path = NULL;
    DIR *dir = NULL;
    struct dirent *entry;

    // first argument should be program name as argv[0]
    args = realloc(args, (nargs + 1) * sizeof(char *));
    if (args == NULL) {
      fprintf(stderr, "Failed to allocate argument list: %s\n",
              strerror(errno));
      exit(1);
    }
    args[0] = strdup(gtk_query_immodules);
    nargs++;

    asprintf(&gtk_immodules_path, "%s/usr/lib/%s/gtk-3.0/3.0.0/immodules", snap,
             arch);
    dir = opendir(gtk_immodules_path);
    while (dir != NULL && (entry = readdir(dir)) != NULL) {
      // if starts with im- and ends with .so then append this to the list of
      // modules
      if ((strncmp(entry->d_name, "im-", strlen("im-")) == 0) &&
          (strlen(entry->d_name) > 3) &&
          (strncmp(entry->d_name + strlen(entry->d_name) - 3, ".so", 3) == 0)) {
        char **args_ = realloc(args, (nargs + 1) * sizeof(char *));
        if (args_ == NULL) {
          fprintf(stderr, "Failed to realloc args list: %s\n", strerror(errno));
          free(args);
          args = NULL;
          nargs = 0;
          break;
        }
        args = args_;
        asprintf(&args[nargs], "%s/%s", gtk_immodules_path, entry->d_name);
        nargs++;
      }

      // add a NULL terminator to the list of arguments
      char **args_ = realloc(args, (nargs + 1) * sizeof(char *));
      if (args_ == NULL) {
        fprintf(stderr, "Failed to realloc args list: %s\n", strerror(errno));
        free(args);
        args = NULL;
        nargs = 0;
        goto fork;
      }
      args = args_;
      args[nargs] = NULL;
    }

  fork: {
    pid_t child = fork();
    if (child == -1) {
      fprintf(stderr, "Failed to fork: %s\n", strerror(errno));
      exit(1);
    }
    if (child == 0) {
      // we are the child - redirect our output to gtk_im_module_file and
      // exec gtk_query_immodules with args as arguments
      int fd = open(gtk_im_module_file, O_CREAT | O_TRUNC | O_WRONLY, 0644);
      if (fd < 0) {
        fprintf(stderr, "Failed to open %s: %s\n", gtk_im_module_file,
                strerror(errno));
        exit(1);
      }
      res = dup2(fd, 1);
      if (res < 0) {
        fprintf(stderr, "Failed to dup2: %s\n", strerror(errno));
        exit(1);
      }
      dbg("Executing %s with args:\n", gtk_query_immodules);
      res = execv(gtk_query_immodules, args);
      if (res < 0) {
        fprintf(stderr, "Failed to exec %s: %s\n", gtk_query_immodules,
                strerror(errno));
        exit(1);
      }
    }
    // wait for child to execute
    waitpid(child, NULL, 0);
  }

    dbg("Setting GTK_IM_MODULE_FILE to %s\n", gtk_im_module_file);
    setenv("GTK_IM_MODULE_FILE", gtk_im_module_file, 1);
  }

  // set GTK_PATH to find gtk modules from the snap and not the host to avoid
  // startup errors like:
  //
  // Gtk-Message: 04:21:40.701: Failed to load module "pk-gtk-module"
  // Gtk-Message: 04:21:40.701: Failed to load module "canberra-gtk-module"
  {
    char *gtk_path;
    asprintf(&gtk_path, "%s/usr/lib/%s/gtk-3.0", snap, arch);
    dbg("Setting GTK_PATH to %s\n", gtk_path);
    setenv("GTK_PATH", gtk_path, 1);
  }

  // set PATH to include binaries from the snap since native comp needs to find
  // as and other similar binaries
  path = getenv("PATH");
  if (path != NULL) {
    char *new_path;
    asprintf(&new_path, "%s:%s/usr/bin", path, snap);
    dbg("Setting PATH to %s\n", new_path);
    setenv("PATH", new_path, 1);
  }

  // setup sylinks so we can create a sysroot for native comp via symlinks to
  // avoid taking disk space
  {
    char *sysroot, *target, *linkpath;
    asprintf(&sysroot, "%s/sysroot", snap_user_common);
    dbg("Creating sysroot %s\n", sysroot);
    res = mkdir(sysroot, S_IRUSR | S_IWUSR | S_IXUSR);
    if (res < 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to create sysroot dir at %s: %s\n", sysroot,
              strerror(errno));
      exit(1);
    }
    asprintf(&target, "%s/usr", snap);
    asprintf(&linkpath, "%s/usr", sysroot);
    unlink(linkpath);
    res = symlink(target, linkpath);
    if (res < 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to symlink %s to %s: %s\n", target, linkpath,
              strerror(errno));
      exit(1);
    }

    // also create lib64 if it exists
    struct stat sb;
    asprintf(&target, "%s/usr/lib64", sysroot);
    res = stat(target, &sb);
    if (res == 0) {
      asprintf(&linkpath, "%s/lib64", sysroot);
      unlink(linkpath);
      res = symlink(target, linkpath);
      if (res < 0 && errno != EEXIST) {
        fprintf(stderr, "Failed to symlink %s to %s: %s\n", target, linkpath,
                strerror(errno));
        exit(1);
      }
    }
  }

  // set GSETTINGS_SCHEMA_DIR for
  // https://github.com/alexmurray/emacs-snap/issues/103 and
  // https://github.com/alexmurray/emacs-snap/issues/104 by taking the host
  // system's gsettings schemas and patching them to match what is expected by
  // the version of GTK etc shipped in the snap - this is a best effort to make
  // sure that the gsettings schemas are compatible between the host system and
  // the snap and also that we respect the host system's settings
  {
    struct override {
      const char *schema;
      const char *key; // if key is not present then the whole schema is
                       // overridden
      int present;
    };
    struct override overrides[] = {
      // https://github.com/alexmurray/emacs-snap/issues/101#issuecomment-2684232893
      {"org.gtk.Settings.FileChooser.gschema.xml", "show-type-column"},
    };
    int i;

    for (i = 0; i < sizeof(overrides) / sizeof(overrides[0]); i++) {
      // for each override, check if key is present in schema on host
      char *schema_path;
      FILE *schema_file;
      char *line = NULL;
      size_t len = 0;

      overrides[i].present = 0;

      asprintf(&schema_path, "/usr/share/glib-2.0/schemas/%s",
               overrides[i].schema);
      schema_file = fopen(schema_path, "r");
      if (schema_file == NULL) {
        dbg("Failed to open %s: %s\n", schema_path, strerror(errno));
        continue;
      }
      while (getline(&line, &len, schema_file) != -1) {
        if (strstr(line, overrides[i].key) != NULL) {
          dbg("Found %s in %s\n", overrides[i].key, overrides[i].schema);
          overrides[i].present = 1;
          free(line);
          line = NULL;
          break;
        }
        free(line);
        line = NULL;
      }
      fclose(schema_file);
      free(schema_path);
    }

    // if any schemas are missing from the host then we need to duplicate the
    // hosts schemas but with the overrides from the snap for any missing ones
    int needed = 0;
    for (i = 0; i < sizeof(overrides) / sizeof(overrides[0]); i++) {
      needed += !overrides[i].present;
    }
    if (needed) {
      char *gsettings_schema_dir, *gschemas_compiled;
      struct stat host_st, snap_st;

      dbg("Setting up GSETTINGS_SCHEMA_DIR for %d overrides\n", needed);
      asprintf(&gsettings_schema_dir, "%s/.cache/schemas", snap_user_common);
      mkdir(gsettings_schema_dir, 0700);

      asprintf(&gschemas_compiled, "%s/gschemas.compiled",
               gsettings_schema_dir);
      res = stat("/usr/share/glib-2.0/schemas/gschemas.compiled", &host_st);
      res |= stat(gschemas_compiled, &snap_st);
      // if the gschemas.compiled file does not exist or the modification time
      // of it is older than the hosts then duplicate the host's gsettings
      // schemas with overrides to the snap's schemas and then compile them all
      if (res != 0 || host_st.st_mtime > snap_st.st_mtime) {
        DIR *dir;
        struct dirent *entry;

        dbg("Compiling gsettings schemas from host\n");
        // remove any existing symlinks etc in the cache dir
        dir = opendir(gsettings_schema_dir);
        while ((entry = readdir(dir)) != NULL) {
          if (entry->d_type == DT_LNK || entry->d_type == DT_REG) {
            char *link_path;
            asprintf(&link_path, "%s/%s", gsettings_schema_dir, entry->d_name);
            unlink(link_path);
          }
        }

        // duplicate the host's gsettings schemas to the snap's cache dir
        dir = opendir("/usr/share/glib-2.0/schemas");
        while ((entry = readdir(dir)) != NULL) {
          if (entry->d_type == DT_REG &&
              // only worry about .enums.xml, gschema.xml or .gschema.override
              // files
              (str_ends_with(entry->d_name, ".enums.xml") ||
               str_ends_with(entry->d_name, ".gschema.xml") ||
               str_ends_with(entry->d_name, ".gschema.override"))) {
            char *target_path, *link_path;
            asprintf(&target_path, "/usr/share/glib-2.0/schemas/%s",
                     entry->d_name);
            asprintf(&link_path, "%s/%s", gsettings_schema_dir, entry->d_name);
            unlink(link_path);
            // symlink so we don't have to copy the schemas
            dbg("Linking %s to %s\n", target_path, link_path);
            res = symlink(target_path, link_path);
            if (res < 0) {
              fprintf(stderr, "Failed to symlink %s to %s: %s\n", target_path,
                      link_path, strerror(errno));
            }
          }
        }
        // link to the schema provided by the snap for any which may be
        // incompatible from the host
        for (i = 0; i < sizeof(overrides) / sizeof(overrides[0]); i++) {
          char *target_path, *link_path;

          if (overrides[i].present) {
            continue;
          }
          asprintf(&target_path, "%s/usr/share/glib-2.0/schemas/%s", snap,
                   overrides[i].schema);
          asprintf(&link_path, "%s/%s", gsettings_schema_dir,
                   overrides[i].schema);
          unlink(link_path);
          dbg("Linking %s to %s\n", target_path, link_path);
          res = symlink(target_path, link_path);
          if (res < 0) {
            fprintf(stderr, "Failed to symlink %s to %s: %s\n", target_path,
                    link_path, strerror(errno));
          }
        }

        // now we need to compile our frankenschema
        unlink(gschemas_compiled);

        pid_t child = fork();
        if (child == -1) {
          fprintf(stderr, "Failed to fork: %s\n", strerror(errno));
          exit(1);
        }
        if (child == 0) {
          // we are the child - exec glib-compile-schemas
          char *glib_compile_schemas;
          asprintf(&glib_compile_schemas, "%s/usr/bin/glib-compile-schemas",
                   snap);
          dbg("Executing %s with %s as schema dir\n", glib_compile_schemas,
              gsettings_schema_dir);
          res = execl(glib_compile_schemas, glib_compile_schemas,
                      gsettings_schema_dir, NULL);
          if (res < 0) {
            fprintf(stderr, "Failed to exec %s: %s\n", glib_compile_schemas,
                    strerror(errno));
            exit(1);
          }
        }
        // wait for child
        waitpid(child, NULL, 0);

        // check for presence of gschemas.compiled file
        res = access(gschemas_compiled, F_OK);
        if (res < 0) {
          fprintf(stderr, "Failed to compile schemas: %s not found\n",
                  gschemas_compiled);
        }
      }

      // set GSETTINGS_SCHEMA_DIR to the snap's cache dir
      dbg("Setting GSETTINGS_SCHEMA_DIR to %s\n", gsettings_schema_dir);
      setenv("GSETTINGS_SCHEMA_DIR", gsettings_schema_dir, 1);
    }
  }

  // create XDG_RUNTIME_DIR/gvfsd if it does not already exist to avoid
  // https://github.com/alexmurray/emacs-snap/issues/101 - seems the
  // GtkFileDialog or similar wants to monitor this directory and complains if
  // it doesn't exist - it may not exist since the host system is running an
  // older version of gvfsd-trash (which seems to own sockets within this
  // directory) but the snap ships a newer gtk which expects it to exist
  {
    char *xdg_runtime_dir, *gvfsd_dir;
    xdg_runtime_dir = getenv("XDG_RUNTIME_DIR");
    if (xdg_runtime_dir) {
      asprintf(&gvfsd_dir, "%s/gvfsd", xdg_runtime_dir);
      dbg("Creating gvfsd dir at %s\n", gvfsd_dir);
      res = mkdir(gvfsd_dir, S_IRUSR | S_IWUSR | S_IXUSR);
      if (res < 0 && errno != EEXIST) {
        fprintf(stderr, "Failed to create gvfsd dir at %s: %s\n", gvfsd_dir,
                strerror(errno));
      }
    }
  }

  // when using a non-pgtk variant of emacs, set LIBGL_ALWAYS_SOFTWARE to 1 to
  // avoid possibly trying to use any native OpenGL libraries from the host
  // which may not be compatible with the snap's version of GTK / X11 etc
  if (strstr(VARIANT, "pgtk") == NULL) {
    dbg("Setting LIBGL_ALWAYS_SOFTWARE to 1 for non-pgtk variant %s\n", VARIANT);
    setenv("LIBGL_ALWAYS_SOFTWARE", "1", 1);
  }

  // finally break out of AppArmor confinement ignoring errors here since this
  // is best effort
  {
    int fd;
    char label[1024] = {0};

    fd = open("/proc/self/attr/current", O_RDONLY | O_CLOEXEC);
    read(fd, label, sizeof(label));
    close(fd);

    // if label starts with snap.emacs. and ends with (complain) then set to
    // unconfined
    if (strncmp(label, "snap.emacs.", strlen("snap.emacs.")) == 0 &&
        strlen(label) > strlen("(complain)") &&
        strncmp(label + strlen(label) - strlen("(complain)"), "(complain)",
                strlen("(complain)")) == 0) {
      // ignore errors here too
      fd = open("/proc/self/attr/current", O_WRONLY | O_APPEND);
      dbg("Changing AppArmor profile to unconfined\n");
      write(fd, "changeprofile unconfined", strlen("changeprofile unconfined"));
      close(fd);
    }
  }

  // finally exec argv if we have one
  if (argc > 1) {
    dbg("Executing %s with args:\n", argv[1]);
    execv(argv[1], &argv[1]);
  }

exit:
  exit(0);
}

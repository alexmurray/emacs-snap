#define _GNU_SOURCE // for asprintf

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// NOTE: in general we don't bother to free any of the dynamically allocated
// memory since this is not a long-lived process so we don't care about memory
// leaks as it will all get cleaned up when the process exits
int main(int argc, char *argv[])
{
  int res;
  struct stat st;
  const char *snap;
  const char *snap_arch;
  const char *snap_user_common;
  char *arch = NULL;
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

  snap = getenv("SNAP");
  if (snap == NULL) {
    fprintf(stderr, "SNAP is not set\n");
    exit(1);
  }

  snap_arch = getenv("SNAP_ARCH");
  if (snap_arch == NULL) {
    fprintf(stderr, "SNAP_ARCH is not set\n");
    exit(1);
  }
  if (strcmp(snap_arch, "amd64") == 0) {
    asprintf(&arch, "x86_64-linux-gnu");
  } else if (strcmp(snap_arch, "armhf") == 0) {
    asprintf(&arch, "arm-linux-gnueabihf");
  } else if (strcmp(snap_arch, "arm64") == 0) {
    asprintf(&arch, "aarch64-linux-gnu");
  } else {
    asprintf(&arch, "%s-linux-gnu", snap_arch);
  }

  snap_user_common = getenv("SNAP_USER_COMMON");
  if (snap_user_common == NULL) {
    fprintf(stderr, "SNAP_USER_COMMON is not set\n");
    exit(1);
  }

  asprintf(&gdk_cache_dir, "%s/.cache", snap_user_common);
  mkdir(gdk_cache_dir, 0700);

  asprintf(&gio_module_dir, "%s/usr/lib/%s/gio/modules", snap,  arch);
  setenv("GIO_MODULE_DIR", gio_module_dir, 1);

  asprintf(&gdk_pixbuf_module_file, "%s/gdk-pixbuf-loaders.cache", gdk_cache_dir);
  setenv("GDK_PIXBUF_MODULE_FILE", gdk_pixbuf_module_file, 1);

  asprintf(&gdk_pixbuf_moduledir, "%s/usr/lib/%s/gdk-pixbuf-2.0/2.10.0/loaders", snap, arch);
  setenv("GDK_PIXBUF_MODULEDIR", gdk_pixbuf_moduledir, 1);

  asprintf(&gdk_pixbuf_query_loaders, "%s/usr/lib/%s/gdk-pixbuf-2.0/gdk-pixbuf-query-loaders", snap, arch);
  res = stat(gdk_pixbuf_query_loaders, &st);
  if (res == 0) {
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
        fprintf(stderr, "Failed to open %s: %s\n", gdk_pixbuf_module_file, strerror(errno));
        exit(1);
      }
      res = dup2(fd, 1);
      if (res < 0) {
        fprintf(stderr, "Failed to dup2: %s\n", strerror(errno));
        exit(1);
      }
      res = execl(gdk_pixbuf_query_loaders, gdk_pixbuf_query_loaders, NULL);
      if (res < 0) {
        fprintf(stderr, "Failed to exec %s: %s\n", gdk_pixbuf_query_loaders, strerror(errno));
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
    asprintf(&cachedir_entry, "	<cachedir>%s</cachedir>\n", fontconfig_cache_dir);
    asprintf(&source_path, "%s/etc/fonts/fonts.conf", snap);
    infd = open(source_path, O_RDONLY);
    if (infd == -1) {
      fprintf(stderr, "Failed to open source fontconfig file %s: %s\n", source_path, strerror(errno));
    }
    outfd = open(fontconfig_file, O_CREAT | O_TRUNC | O_WRONLY | O_CLOEXEC, 0644);
    if (outfd == -1) {
      fprintf(stderr, "Failed to open dest fontconfig file %s: %s\n", fontconfig_file, strerror(errno));
    }
    res = fstat(infd, &st);
    if (res == -1) {
      fprintf(stderr, "Failed to stat infd: %s\n", strerror(errno));
    }
    infile = fdopen(infd, "r");
    if (infile == NULL) {
      fprintf(stderr, "Failed to open %s for streaming: %s\n", source_path, strerror(errno));
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
  asprintf(&gtk_query_immodules, "%s/usr/lib/%s/libgtk-3-0/gtk-query-immodules-3.0", snap, arch);
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
      fprintf(stderr, "Failed to allocate argument list: %s\n", strerror(errno));
      exit(1);
    }
    args[0] = strdup(gtk_query_immodules);
    nargs++;

    asprintf(&gtk_immodules_path, "%s/usr/lib/%s/gtk-3.0/3.0.0/immodules", snap, arch);
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

    fork:
    {
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
          fprintf(stderr, "Failed to open %s: %s\n", gtk_im_module_file, strerror(errno));
          exit(1);
        }
        res = dup2(fd, 1);
        if (res < 0) {
          fprintf(stderr, "Failed to dup2: %s\n", strerror(errno));
          exit(1);
        }
        res = execv(gtk_query_immodules, args);
        if (res < 0) {
          fprintf(stderr, "Failed to exec %s: %s\n", gtk_query_immodules, strerror(errno));
          exit(1);
        }
      }
      // wait for child to execute
      waitpid(child, NULL, 0);
    }
  }

  // set PATH to include binaries from the snap since native comp needs to find
  // as and other similar binaries
  path = getenv("PATH");
  if (path != NULL)
  {
    char *new_path;
    asprintf(&new_path, "%s:%s/usr/bin", path, snap);
    setenv("PATH", new_path, 1);
  }

  // setup sylinks so we can create a sysroot for native comp via symlinks to
  // avoid taking disk space
  {
    char *sysroot, *target, *linkpath;
    asprintf(&sysroot, "%s/sysroot", snap_user_common);
    res = mkdir(sysroot, S_IRUSR | S_IWUSR | S_IXUSR);
    if (res < 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to create sysroot dir at %s: %s\n", sysroot, strerror(errno));
      exit(1);
    }
    asprintf(&target, "%s/usr", snap);
    asprintf(&linkpath, "%s/usr", sysroot);
    unlink(linkpath);
    res = symlink(target, linkpath);
    if (res < 0 && errno != EEXIST) {
      fprintf(stderr, "Failed to symlink %s to %s: %s\n", target, linkpath, strerror(errno));
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
        fprintf(stderr, "Failed to symlink %s to %s: %s\n", target, linkpath, strerror(errno));
        exit(1);
      }
    }
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
        strncmp(label + strlen(label) - strlen("(complain)"),
                "(complain)", strlen("(complain)")) == 0) {
      // ignore errors here too
      fd = open("/proc/self/attr/current", O_WRONLY | O_APPEND);
      write(fd, "changeprofile unconfined", strlen("changeprofile unconfined"));
      close(fd);
    }
  }

  // finally exec argv if we have one
  if (argc > 1) {
    execv(argv[1], &argv[1]);
  }

 exit:
  exit(0);
}


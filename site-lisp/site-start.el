;; we don't want subprocesses inheriting the snap specific GDK_PIXBUF and
;; FONTCONFIG environment so make sure they are effective unset
(setenv "GDK_PIXBUF_MODULE_FILE")
(setenv "GDK_PIXBUF_MODULEDIR")
(setenv "FONTCONFIG_FILE")
;; ensure native-comp can find the compiler
(setq comp-native-driver-options '("-B/snap/emacs/current/usr/lib/gcc/x86_64-linux-gnu/10/"))

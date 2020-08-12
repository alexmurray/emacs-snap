;; we don't want subprocesses inheriting the snap specific GDK_PIXBUF and
;; FONTCONFIG environment so make sure they are effective unset
(setenv "GDK_PIXBUF_MODULE_FILE")
(setenv "GDK_PIXBUF_MODULEDIR")
(setenv "FONTCONFIG_FILE")

;;; site-start.el --- Make Emacs work correctly as a snap

;;; Commentary:

;; This file contains the various settings to allow Emacs to function
;; correctly as a strictly confined snap

;;; Code:

;; since the Emacs snap is under classic confinement, it runs in the host
;; systems mount namespace and hence will use the host system's PATH
;; etc. As such, we don't want subprocesses launched by Emacs to inherit
;; the snap specific GIO_MODULE_DIR, GDK_PIXBUF and FONTCONFIG environment
;; as they likely will be linked against different libraries than what the
;; Emacs snap base snap is using. So make sure they are effectively unset.
(setenv "GIO_MODULE_DIR")
(setenv "GDK_PIXBUF_MODULE_FILE")
(setenv "GDK_PIXBUF_MODULEDIR")
(setenv "FONTCONFIG_FILE")

;; ensure native-comp can find the compiler
(eval-when-compile
  (require 'comp))
(let ((snap (file-name-as-directory (getenv "SNAP"))))
  (setq native-comp-driver-options (list (concat "--sysroot=" snap)
                                         (concat "-B" snap "usr/lib/gcc/"))))

(provide 'site-start)
;;; site-start.el ends here

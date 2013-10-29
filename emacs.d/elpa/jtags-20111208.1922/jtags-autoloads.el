;;; jtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (jtags-mode) "jtags" "jtags.el" (21103 23010 0
;;;;;;  0))
;;; Generated autoloads from jtags.el
 (add-hook 'java-mode-hook 'jtags-mode)

(autoload 'jtags-mode "jtags" "\
Toggle jtags mode.
With arg, turn jtags mode on if arg is positive.

When jtags mode is enabled, a number of improved tags lookup commands are
available, as shown below. jtags mode provides commands for looking up the
identifier before or around point, completing partly typed identifiers, and
managing tags table files.

\\{jtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (jtags-extras) "jtags-extras" "jtags-extras.el"
;;;;;;  (21103 23010 0 0))
;;; Generated autoloads from jtags-extras.el
 (add-hook 'java-mode-hook 'jtags-extras)

(autoload 'jtags-extras "jtags-extras" "\
Load `jtags-extras'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("jtags-pkg.el") (21103 23010 198486 0))

;;;***

(provide 'jtags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jtags-autoloads.el ends here

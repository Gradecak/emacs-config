(defvar default-gc-cons-threshold 16777216 )

;; make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;disable shitty UI elements
(setq default-frame-alist
      '((fullscreen . maximized)
	(font . "FiraCode 12")
        (set-language-environment "UTF-8")
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)))

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(provide 'early-init)
;;; early-init.el ends here

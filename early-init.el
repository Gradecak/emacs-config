;; make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;disable shitty UI elements
(setq default-frame-alist
      '((fullscreen . maximized)
        (alpha . (90 90))
	(font . "Iosevka 13")
        (set-language-environment "UTF-8")
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)))

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(provide 'early-init)
;;; early-init.el ends here

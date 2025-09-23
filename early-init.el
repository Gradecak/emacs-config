;; -*- lexical-binding: t; -*-

;; make garbage collector less invasive
(setq package-enable-at-startup nil)
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
;; run garbage collection when focus changes
(add-function :after after-focus-change-function
	      (defun me/garbage-collect-maybe ()
		(unless (frame-focus-state)
		  (garbage-collect))))

(setq load-prefer-newer t)
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb
(setq process-adaptive-read-buffering nil)

;;disable shitty UI elements
(setq default-frame-alist
      '((fullscreen . maximized)
        (alpha . (100 100))
	(font . "Iosevka 13")
        (set-language-environment "UTF-8")
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)))

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(when (equal system-type 'darwin)
  ;; only way I know how to prevent the macos specific behaviour of windows always being
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(provide 'early-init)
;;; early-init.el ends here

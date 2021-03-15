(defvar default-gc-cons-threshold 16777216 )

;; make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; disable shitty UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(setq initial-frame-alist
        '((width . 80)
          (tool-bar-lines . 0)
          (font . "Iosevka 11")
          (set-language-environment "UTF-8")))

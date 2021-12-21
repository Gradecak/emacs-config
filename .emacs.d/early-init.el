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

;; (add-hook
;;  'server-after-make-frame-hook
;;  (let (done)
;;    (lambda ()
;;      (unless done
;;        ;; still set done to true even if we hit a bug (otherwise we
;;        ;; can never open a frame to see the problem)
;;        (setq done t)
;;        (initialise-emoji-font)))))

(setq default-frame-alist
        '((width . 80)
          (tool-bar-lines . 0)
          (font . "FiraCode 13")
          (set-language-environment "UTF-8")))



(provide 'early-init)
;;; early-init.el ends here

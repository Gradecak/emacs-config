;; -*- lexical-binding: t; -*-
(require 'json)

(defconst grpclient--empty-line-regexp "^\\s-*$")
(defconst grpclient--use-var-regexp
  "^\\(:[^: \n]+\\)$")
(defconst grpclient--comment-separator "#")
(defconst grpclient--comment-start-regexp (concat "^" grpclient--comment-separator))
(defconst grpclient--comment-not-regexp (concat "^[^" grpclient--comment-separator "]"))
(defconst grpclient--var-regexp
  (concat "^\\(:[^:= ]+\\)[ \t]*\\(:?\\)=[ \t]*\\(<<[ \t]*\n\\(\\(.*\n\\)*?\\)" grpclient--comment-separator "\\|\\([^<].*\\)$\\)"))
(defconst grpclient--request-regexp
  "^\\(GRPC\\)\s\\(.+?\\)\s\\(.+?\\)$")
(defconst grpclient--header-regexp
  "^\\([^](),/:;@[\\{}= \t]+\\): \\(.*\\)$")

;; used for font locking - not in actual behaviour
(defconst grpclient--svar-regexp
  "^\\(:[^:= ]+\\)[ \t]*=[ \t]*\\(.+?\\)$")
(defconst grpclient--evar-regexp
  "^\\(:[^: ]+\\)[ \t]*:=[ \t]*\\(.+?\\)$")
(defconst grpclient--mvar-regexp
  "^\\(:[^: ]+\\)[ \t]*:?=[ \t]*\\(<<\\)[ \t]*$")


;; faces
(defgroup grpclient-faces nil
  "Faces used in Grpclient Mode"
  :group 'grpclient
  :group 'faces)

(defface grpclient-variable-name-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for variable name."
  :group 'grpclient-faces)

(defface grpclient-variable-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for variable value (string)."
  :group 'grpclient-faces)

(defface grpclient-variable-elisp-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'grpclient-faces)

(defface grpclient-variable-multiline-face
  '((t (:inherit font-lock-doc-face)))
  "Face for multi-line variable value marker."
  :group 'grpclient-faces)

(defface grpclient-variable-usage-face
  '((t (:inherit grpclient-variable-name-face)))
  "Face for variable usage (only used when headers/body is represented as a single variable, not highlighted when variable appears in the middle of other text)."
  :group 'grpclient-faces)

(defface grpclient-request-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for GRPC keyword."
  :group 'grpclient-faces)

(defface grpclient-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for variable value (Emacs lisp)."
  :group 'grpclient-faces)

(defface grpclient-header-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTTP header name."
  :group 'grpclient-faces)

(defface grpclient-header-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTTP header value."
  :group 'grpclient-faces)

(defface grpclient-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for GRPC method."
  :group 'restclient-faces)

(defface grpclient-proto-face
  '((t (:inherit font-lock-constant-face)))
  "Face for GRPC method."
  :group 'restclient-faces)


(defun grpclient--current-min ()
  (save-excursion
    (beginning-of-line)
    ;; skip over comment lines
    (if (looking-at grpclient--comment-start-regexp)
        (if (re-search-forward grpclient---comment-not-regexp (point-max) t)
            (point-at-bol)
          (point-max))
      ;; 'blocks' are separated by comments min point of a block is after previous comment or start of buffer
      (if (re-search-backward grpclient--comment-start-regexp (point-min) t)
          (point-at-bol 2)
        (point-min)))))

(defun grpclient--current-max ()
  (save-excursion
    (if (re-search-forward grpclient--comment-start-regexp (point-max) t)
        (max (- (point-at-bol) 1) 1)
      (progn (goto-char (point-max))
             (if (looking-at "^$") (- (point) 1) (point))))))


(defun grpclient--chop (text)
  (if text (replace-regexp-in-string "\n$" "" text) nil))

(defun grpclient--substitute-vars (vars input-str)
  (if vars
      (let* ((var-names (mapcar 'car vars))
            (substitutions (regexp-opt var-names))
            (current-val input-str)
            (pass 3))
        (while (> pass 0)
          (setq pass (- pass 1))
          (setq current-val (replace-regexp-in-string substitutions (lambda (var) (cdr (assoc var vars))) current-val t t)))
        current-val)
    input-str))

(defun grpclient--eval-var (string)
  (with-output-to-string (princ (eval (read string)))))

(defun grpclient--find-vars-before-point ()
  (let ((vars nil)
        (current-point (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp grpclient--var-regexp current-point t)
        (let ((name (match-string-no-properties 1))
              (is-lisp-expr (> (length (match-string 2)) 0))
              (value (or (grpclient--chop (match-string-no-properties 4))
                         (match-string-no-properties 3))))
          (setq vars (cons (cons name (if is-lisp-expr (grpclient--eval-var value) value)) vars))))
      vars)))

(defun grpclient--substitute-vars-in-header (vars header)
  (cons (car header) (grpclient--substitute-vars vars (cdr header))))

(defun grpclient--make-header (&optional string)
  (cons (match-string-no-properties 1 string) (match-string-no-properties 2 string)))

(defun grpclient--parse-headers-from-string (string)
  (let ((start 0)
        (headers '()))
    (while (string-match grpclient--header-regexp string start)
      (setq headers (cons (grpclient--make-header string) headers)
            start (match-end 0)))
    headers))

(defun grpclient--parse-body (vars)
  (let* ((cmax (grpclient--current-max))
         (request-body (string-trim
                        (buffer-substring-no-properties (min (point) cmax) cmax))))
    (grpclient--substitute-vars vars request-body)))

(defun grpclient--build-grpc-args (vars address method headers body)
  (let* ((headers (mapcar
                   (lambda (header) (concat "-H '" (format "%s: %s" (car header) (cdr header)) "'"))
                   headers))
         (parts (append headers
                        (split-string (cdr (assoc-string ":flags" vars)) "\n")
                        (list
                         (when body (concat "-d '" (encode-coding-string body 'utf-8) "'"))
                         address
                         (or method "")))))
    (seq-filter 'identity parts)))

(defun grpclient--prepare-grpc-request ()
  (save-excursion
    (goto-char (grpclient--current-min))
    (when (re-search-forward grpclient--request-regexp (point-max) t)
      (let ((raw-address (match-string-no-properties 2))
            (raw-method (match-string-no-properties 3))
            (vars (grpclient--find-vars-before-point))
            headers '())
        (forward-line) ;; next line is either headers or request body
        (while (cond
                ;; if a single header is set
                ((and (looking-at grpclient--header-regexp) (not (looking-at grpclient--empty-line-regexp)))
                 (setq headers (cons (grpclient--substitute-vars-in-header vars (grpclient--make-header)) headers)))
                ;; if headers are set from variable
                ((looking-at grpclient--use-var-regexp)
                 (setq headers
                       (append headers (grpclient--parse-headers-from-string (grpclient--substitute-vars vars (match-string 1)))))))
          (forward-line))
        (when (looking-at grpclient--empty-line-regexp)
          (forward-line))
        (let ((body (grpclient--parse-body vars))
              (address (grpclient--substitute-vars vars raw-address))
              (method (grpclient--substitute-vars vars raw-method)))
          (append '("grpcurl") (grpclient--build-grpc-args vars address method headers body)))))))


(defun grpclient-copy-grpcurl-string ()
  (interactive)
  (kill-new (mapconcat 'identity (grpclient--prepare-grpc-request) " ")))

(defun grpclient--body-start ()
  (save-excursion
    (goto-char (grpclient--current-min))
    (forward-line)
    (while (or (looking-at grpclient--header-regexp)
               (looking-at grpclient--use-var-regexp)
               (looking-at grpclient--empty-line-regexp))
      (forward-line))
    (point)))


;;;###autoload
(defun grpclient-format-body ()
  (interactive)
  (save-excursion
    (let ((body-start (grpclient--body-start))
          (body-end))
      (goto-char body-start)
      (ignore-errors (forward-sexp))
      (json-pretty-print body-start (point))
      )))


;;;###autoload
(defun grpclient-do-current ()
  (interactive)
  (let* ((buff-name "*GRPClient OUT*")
         (args (grpclient--prepare-grpc-request))
         (command (mapconcat #'identity args " ")))
    (call-process-shell-command command nil buff-name )
    (display-buffer buff-name)))



(defconst grpclient-mode-keywords
  (list (list grpclient--request-regexp '(1 'grpclient-keyword-face) '(2 'grpclient-url-face) '(3 'grpclient-proto-face))
        (list grpclient--use-var-regexp '(1 'grpclient-variable-usage-face))
        (list grpclient--svar-regexp '(1 'grpclient-variable-name-face) '(2 'grpclient-variable-string-face))
        (list grpclient--evar-regexp '(1 'grpclient-variable-name-face) '(2 'grpclient-variable-elisp-face t))
        (list grpclient--mvar-regexp '(1 'grpclient-variable-name-face) '(2 'grpclient-variable-multiline-face t))
        (list grpclient--header-regexp '(1 'grpclient-header-name-face t) '(2 'grpclient-header-value-face t))))

(defconst grpclient-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defvar grpclient-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'grpclient-do-current)
    ;; (define-key map (kbd "C-c C-n") 'restclient-jump-next)
    ;; (define-key map (kbd "C-c C-p") 'restclient-jump-prev)
    (define-key map (kbd "C-c C-u") 'grpclient-copy-grpcurl-string)
    map)
  "Keymap for grpclient-mode.")

;;;###autoload
(define-derived-mode grpclient-mode fundamental-mode "GRPC Client"
  "Activate grpclient mode"
  (set (make-local-variable 'font-lock-defaults) '(grpclient-mode-keywords)))


(provide 'grpc)

;;; curl.el --- Make https work via curl on WSL      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `url-https' does not work on Windows Subsystem for Linux.
;; This package replaces `url-http' (which is called by `url-https')
;; with the external program curl for retrieving data.

;;; Code:

(defcustom curl-program "curl"
  "Curl program."
  :group 'curl
  :type 'string)

(defcustom curl-options '("-i" "--output" "-")
  "Curl program."
  :group 'curl
  :type '(repeat string))

(defvar-local curl-callback nil
  "Callback for `curl-http'.")

(defvar-local curl-cbargs nil
  "Callback arguments for `curl-http'.")

(defvar-local curl-content-type nil
  "Content type of the retrieved url data in current buffer.")

(defvar-local curl-head-complete nil
  "Http head is completely recieved.")

(defmacro curl-local-setq (buf sym val)
  "Set SYM locally in BUF to VAL."
  (declare (debug (sexp symbolp sexp)))
  (let ((value (make-symbol "value")))
    `(let ((,value ,val)) ;; Evaluation of the value in the original buffer.
       (with-current-buffer ,buf ;; But setting in BUF.
	 (setq-local ,sym ,value)))))

(defun curl-http-generic-filter (proc string)
  "Apply `url-http-generic-filter' but remove ^M from STRING before.
When retrieving binary data ^M is only stripped from the header.
Just pass PROC to `url-http-generic-filter."
  (let ((proc-buf (process-buffer proc)))
    (url-http-generic-filter
     proc
     (if (with-current-buffer proc-buf curl-head-complete)
	 string
       (with-temp-buffer
	 (set-buffer-multibyte nil)
	 (set-buffer-file-coding-system 'no-conversion)
	 (insert string)
	 (goto-char (point-min))
	 (let* ((pos (save-excursion
		       (re-search-forward "\r?\n\r?\n" nil 'noErr)))
		(end (and pos (set-marker (make-marker) pos))))
	   (unwind-protect
	       (progn
		 (when end
		   (curl-local-setq proc-buf curl-head-complete (buffer-substring (point-min) end)))
		 (when (re-search-forward "^Content-Type:[[:space:]]*\\([^[:space:]]+?\\)$\r?$" end 'noErr)
		   (curl-local-setq proc-buf curl-content-type (match-string 1)))
		 (goto-char (point-min))
		 (while (search-forward "\r" end 'noErr)
		   (replace-match "")))
	     (when end
	       (set-marker end nil))))
	 (buffer-string))))))

(defvar curl-copy-process-buffer nil
  "Copy process buffer contents into here.
Only for debugging.")

(defun curl-process-sentinel (proc state)
  "Call `curl-callback' when PROC finished with STATE \"finish\"."
  (unless (process-live-p proc)
    (if (string-match "finish" state)
	(progn ;; regular end
	  (with-current-buffer (process-buffer proc)
	    (message "Running curl-callback in buffer %S" (current-buffer))
	    (when (stringp curl-copy-process-buffer)
	      (with-current-buffer (generate-new-buffer curl-copy-process-buffer)
		(delete-region (point-min) (point-max))
		(set-buffer-multibyte nil)
		(set-buffer-file-coding-system 'no-conversion)
		(insert (with-current-buffer (process-buffer proc)
			  (buffer-string)))))
	    (goto-char (point-max))
	    (insert "\nFinished")
	    (apply curl-callback curl-cbargs)))
      (error "Process %s aborted" proc))))

(defvar url-http-after-change-function)
(make-variable-buffer-local 'url-http-after-change-function)

(defun curl-http (url callback cbargs &optional retry-buffer &rest _ignored)
  "Replacement for `url-http' in WSL.
Retrieve URL and run CALLBACK with status and CBARGS as arguments.
Reuse RETRY-BUFFER if that is non-nil."
  (when (url-p url)
    (setq url (url-recreate-url url)))
  (let* ((buffer (or retry-buffer
		     (generate-new-buffer
                      (format " *%s*" url))))
	 (proc (with-current-buffer buffer
		 (set-buffer-multibyte nil)
		 (set-buffer-file-coding-system 'no-conversion t)
		 (setq-local curl-callback callback)
		 (setq-local curl-cbargs cbargs)
		 (setq-local url-http-after-change-function #'url-http-simple-after-change-function)
		 (message "%S buffer: %S" url buffer)
		 (funcall #'make-process
			  :name (format "*%s*" url)
			  :buffer buffer
			  :command
			  (append
			   (list curl-program)
			   curl-options;; include http response header
			   (list url))
			  :coding 'no-conversion))))
    (message "curl-http, buffer: %S" buffer)
    (set-process-sentinel proc #'curl-process-sentinel)
    (set-process-filter proc #'curl-http-generic-filter)
    buffer))

(unless (executable-find curl-program)
  (user-error "Curl program %s is not available"
	      curl-program))

(advice-add 'url-http :override #'curl-http)

(provide 'curl)
;;; curl.el ends here

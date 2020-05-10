;;; auto-save-async.el --- Auto save asynchronously  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: files

;; Version: 1.0.8
;; Package-Requires: ((emacs "24.3") (async "1.9.4") (switch-buffer-functions "0.0.1"))

;; URL: https://github.com/ROCKTAKEY/auto-save-async

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
;;
;;; Auto save  asynchronously.
;;   Auto save asynchronously, with idle-timer, counting input events,
;;   and switching buffer.
;;; How to Use?
;;   Install and eval those:
;;
;;   (require 'auto-save-async)
;;   (auto-save-async-mode 1)
;;
;;   Now each buffer is auto-saved asynchronously.
;;
;;; Customs
;;;; ~auto-save-async-interval~
;;    Number of input events between auto-save-async.
;;    Zero means auto-save-async do not run by input events' number.
;;;; ~auto-save-async-timeout~
;;    Number of seconds idle time before auto-save-async.
;;    Zero means auto save-async do not run by idle time.
;;;; ~auto-save-async-file-name-transforms~
;;    File name transformer on auto-save-async.
;;    See `auto-save-file-name-transforms', because `make-auto-save-file-name'
;;    is used internally.
;;;; ~auto-save-async-save-when-switch-buffer~
;;    Auto save asynchronously when switching buffer or not.
;;

;;; Code:
(require 'async)
(require 'switch-buffer-functions)

(defgroup auto-save-async nil
  "Group for auto-save-async."
  :group 'files)

(defcustom auto-save-async-interval 300
  "Number of input events between auto-save-async.
Zero means auto-save-async do not run by input events' number."
  :group 'auto-save-async
  :type 'integerp)

(defcustom auto-save-async-timeout 5
  "Number of seconds idle time before auto-save-async.
Zero means auto save-async do not run by idle time."
  :group 'auto-save-async
  :type 'numberp)

(defcustom auto-save-async-file-name-transforms
  auto-save-file-name-transforms
  "File name transformer on auto-save-async.
See `auto-save-file-name-transforms', because `make-auto-save-file-name'
is used internally."
  :group 'auto-save-async
  :type '(repeat (list (string :tag "Regexp")
                       (string :tag "Replacement")
                       (boolean :tag "Uniquify"))))

(defcustom auto-save-async-save-when-switch-buffer t
  "Auto save async when switch buffer or not."
  :group 'auto-save-async
  :type 'boolean)

(defcustom auto-save-async-show-message t
  "Show message when auto save async is started and finished.
If nil, show no message including error.
If symbol `error-only', show error messages only.
If other non-nil value, show all messages."
  :group 'auto-save-async
  :type  '(choice (const t) (const error-only) (const nil)))


;; Inner vars
(defvar auto-save-async--timer nil)

(defvar auto-save-async--counter 0)

(defvar-local auto-save-async--buffer-file-name nil)


;;;###autoload
(defun auto-save-async-save ()
  "Auto save asynchronously."
  (interactive)
  (let ((str
         (save-restriction
           (widen)
           (save-excursion
             (unless (or
                      (not (buffer-file-name))
                      (not (buffer-modified-p))
                      (= (point-max) (point-min))
                      find-file-literally
                      buffer-read-only
                      (null auto-save-async--buffer-file-name))
               (buffer-substring-no-properties (point-max) (point-min)))))))
    (when str
      (when (and auto-save-async-show-message
                 (not (eq auto-save-async-show-message 'error-only)))
        (message "Auto save async..."))
      (async-start
       `(lambda ()
          (condition-case err
              (with-temp-buffer
                (insert ,str)
                (write-file ,(eval auto-save-async--buffer-file-name))
                nil)
            (error-message-string err)))
       `(lambda (result)
          (with-current-buffer ,(current-buffer)
            (setq buffer-saved-size ,(length str)))
          (when auto-save-async-show-message
            (if result
                (display-warning 'auto-save-async :error result)
              (unless (eq auto-save-async-show-message 'error-only)
                (message "Auto save async done.")))))))))

;; Inner functions
(defun auto-save-async--count-and-save ()
  "Count input events and save if it's over `auto-save-async-interval'."
  (when (and (not (eq auto-save-async-interval 0))
             (>= (setq auto-save-async--counter (1+ auto-save-async--counter))
                 auto-save-async-interval))
    (auto-save-async-save)
    (setq auto-save-async--counter 0)))

(defun auto-save-async--switch-buffer (before _)
  "Run `auto-save-async-save' after `switch-buffer' from BEFORE."
  (when (and auto-save-async-save-when-switch-buffer (stringp before))
    (with-current-buffer before
      (auto-save-async-save))))


;;;###autoload
(define-minor-mode auto-save-async-mode
  "Auto save asynchronously."
  nil
  "AS-async"
  nil
  :group 'auto-save-async
  :global t
  (if auto-save-async-mode
      (progn
        (let* ((lexical-binding nil))
          (setq auto-save-async--buffer-file-name
                (let ((auto-save-file-name-transforms
                       auto-save-async-file-name-transforms))
                  (make-auto-save-file-name))))
        (setq
         auto-save-async--timer
         (unless (eq 0 auto-save-async-timeout)
           (run-with-idle-timer auto-save-async-timeout t #'auto-save-async-save)))
        (add-hook 'post-command-hook #'auto-save-async--count-and-save)
        (add-hook 'switch-buffer-functions  #'auto-save-async--switch-buffer))
    (unless auto-save-async--timer (cancel-timer auto-save-async--timer))
    (setq auto-save-async--timer nil)
    (remove-hook 'post-command-hook #'auto-save-async--count-and-save)
    (remove-hook 'switch-buffer-functions  #'auto-save-async--switch-buffer)))

(provide 'auto-save-async)
;;; auto-save-async.el ends here

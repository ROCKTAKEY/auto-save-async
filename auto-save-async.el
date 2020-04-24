;;; auto-save-async.el --- Auto save asynchronously.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: files

;; Version: 0.0.6
;; Package-Requires: ((async "1.9.4") (switch-buffer-functions "0.0.1"))

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

(defvar auto-save-async--timer nil)

(defvar auto-save-async--counter 0)

(defvar-local auto-save-async--buffer-file-name nil)

(defun auto-save-async-save ()
  "Auto save asynchronously."
  (interactive)
  (let ((str
         (save-restriction
           (widen)
           (save-excursion
             (unless (or
                      (not (buffer-file-name))
                      (= (point-max) (point-min))
                      find-file-literally
                      buffer-read-only
                      (null auto-save-async--buffer-file-name))
               (buffer-substring-no-properties (point-max) (point-min)))))))
    (when str
      (message "Auto save async...")
      (async-start
       `(lambda ()
          (with-temp-buffer
            (insert ,str)
            (write-file ,(eval auto-save-async--buffer-file-name))))
       `(lambda (result)
          (with-current-buffer ,(current-buffer)
            (setq buffer-saved-size ,(length str)))
          (message "Auto save async done. %S" result))))))

(defun auto-save-async--count-and-save ()
  (when (>= (setq auto-save-async--counter (1+ auto-save-async--counter))
            auto-save-async-timeout)
    (auto-save-async-save)
    (setq auto-save-async--counter 0)))

(defun auto-save-async--switch-buffer (before _)
  (when auto-save-async-save-when-switch-buffer
   (with-current-buffer before
    (auto-save-async-save))))

(define-minor-mode auto-save-async-mode
  "Auto save asynchronously."
  :lighter "AS-async"
  :group 'auto-save-async
  :global t
  (if auto-save-async-mode
      (progn
        (let* ((lexical-binding nil))
          (setq auto-save-async-buffer-file-name
                (let ((auto-save-file-name-transforms
                       auto-save-async-file-name-transforms))
                  (make-auto-save-file-name))))
        (setq
         auto-save-async--timer
         (unless (eq 0 auto-save-async-timeout)
          (run-with-idle-timer auto-save-async-timeout #'auto-save-async-save)))
        (add-hook 'post-command-hook #'auto-save-async--count-and-save)
        (add-hook 'switch-buffer-functions  #'auto-save-async--switch-buffer))
    (unless auto-save-async--timer (cancel-timer auto-save-async--timer))
    (setq auto-save-async--timer nil)
    (remove-hook 'post-command-hook #'auto-save-async--count-and-save)
    (remove-hook 'switch-buffer-functions  #'auto-save-async--switch-buffer)))

(provide 'auto-save-async)
;;; auto-save-async.el ends here

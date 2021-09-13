;;; magit-smerge.el ---  Call smerge commands from magit diff buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Mark Dawson

;; Author: Mark Dawson <markgdawson@gmail.com>
;; URL: http://github.com/markgdawson/magit-smerge.el
;; Version: 1.6-pre
;; Package-Requires: ((emacs "26.1") (magit "2.13.0"))
;; Keywords: magit, vc

;;; Commentary:

;; This package enables the smerge keybindings in the magit diff buffer.  Currently only
;; `magit-smerge-keep-lower` (`C-c ^ l`) and `magit-smerge-keep-upper` (`C-c ^ u`)
;; are implemented.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'magit)

(defun magit-smerge--goto-line (line)
  "Goto LINE in current buffer."
  (widen)
  (goto-char (point-min))
  (forward-line (- line 1)))

(defun magit-smerge--funcall-at-buffer-point (fn)
  "Call function FN from buffer location at point and save file.

User will be prompted to save before running FN if the file has modifications."
  (save-window-excursion
    (let ((file (magit-file-at-point t t))
          (line (magit-diff-hunk-line (magit-diff-visit--hunk) nil)))
      (with-current-buffer (find-file-noselect file)
        (if (buffer-modified-p (current-buffer))
            (user-error (format "Buffer %s is already modified"
                                (buffer-name (current-buffer)))))

        (magit-smerge--goto-line line)
        (funcall fn)
        (save-buffer)))))

;;;###autoload
(defun magit-smerge-keep-upper ()
  "Call `smerge-keep-upper` from hunk at point in magit diff buffer.

File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-upper)
  (magit-refresh))

;;;###autoload
(defun magit-smerge-keep-lower ()
  "Call `smerge-keep-lower` from hunk at point in magit diff buffer.

File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-lower)
  (magit-refresh))

;;;###autoload
(defun magit-smerge-setup ()
  "Setup magit-smerge keybindings."
  (interactive)
  (define-key magit-hunk-section-map (kbd "C-c ^ l") #'magit-smerge-keep-lower)
  (define-key magit-hunk-section-map (kbd "C-c ^ u") #'magit-smerge-keep-upper))

(provide 'magit-smerge)
;;; magit-smerge.el ends here

;;; magit-smerge.el --- Allow calling smerge commands from magit diff buffer  -*- lexical-binding: t; -*-

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

(defun magit-smerge--funcall-at-buffer-point (fn)
  "Call function FN from buffer location at point.

File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (save-window-excursion
    (let ((file (magit-file-at-point t t))
          (line (magit-diff-hunk-line (magit-diff-visit--hunk) nil)))
      (with-current-buffer (find-file-noselect file)
        (let ((modified (buffer-modified-p (current-buffer)))
              (buffer-name (buffer-name (current-buffer))))
          (widen)
          (goto-char (point-min))
          (forward-line (- line 1))
          (funcall fn)
          (if (or (not modified)
                  (yes-or-no-p (format "Buffer %s was already modified. Save it?"
                                       buffer-name)))
              (progn
                (save-buffer)
                (message "Buffer %s saved." buffer-name))
            (message "Buffer %s not saved due to existing modifications." buffer-name)))))
    (magit-refresh)))

;;;###autoload
(defun magit-smerge-keep-upper ()
  "Call `smerge-keep-upper` from hunk at point in magit diff buffer.

File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-upper))

;;;###autoload
(defun magit-smerge-keep-lower ()
  "Call `smerge-keep-lower` from hunk at point in magit diff buffer.

File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-lower))

;;;###autoload
(defun magit-smerge-setup ()
  "Setup magit-smerge keybindings."
  (interactive)
  (define-key magit-hunk-section-map (kbd "C-c ^ l") #'magit-smerge-keep-lower)
  (define-key magit-hunk-section-map (kbd "C-c ^ u") #'magit-smerge-keep-upper))

(provide 'magit-smerge)
;;; magit-smerge.el ends here

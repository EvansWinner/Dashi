q;;; dashi.el --- quasi-dashboard for Emacs   -*- lexical-binding: t; -*-
;; Copyright (C) 2025  Evans Winner

;; Author: Evans Winner <evans.winner@gmail.com>
;; Keywords: convenience, tools

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

;; Do M-x dashi to get a temporary buffer with a "dashboard" of
;; "widgets" containing interesting facts. Write your own functions
;; and add them to dashi/widget-list and otherwise modify
;; dabbshi/widget-list to customize the output.

;; "Widgets" (groups of one or more peices of interesting information)
;; are just displayed in the buffer from top to bottom, in the order
;; in which they appear in dashi/widget-list.

;; Items are the actual functions that return the interesting
;; information. Widgets should call items. A widget can call as many
;; items as you want.

;;; Code:

;;; Vars

;; Each is a list (function title decorator) where function is a
;; function that returns an unadorned string with the datum you want
;; shown, title is the title of the widget, and decorator is a
;; function that decorates the widget in some way (e.g., a boxquote
;; function or similar). decorator may be nil for no decorator, title
;; may be nil for no title.  Example: (time-of-day "Current Time: "
;; dashi/box)

;; Sections: Emacs, buffers, time/date, files, folders, memory,
;; features, load-path, processes, timers, global modes,
;; functions/symbols

(defvar dashi/decorator-function 'dashi/decor/box)

(defvar dashi/widget-list 
  '(
    (dashi/widget/emacs "Emacs" "💻")
    (dashi/widget/buffers "Buffers" "📃")
    (dashi/widget/time "Time" "🕑")
    (dashi/widget/directories "Directories" "🔢")
    ))

(defvar dashi/buffer-name "*dashi*"




)
(defvar dashi/title
  (concat "📊 "
	  (propertize "Dashi Emacs Dashboard" 'face '(:weight bold :underline t))
	  " 📊\n[r]efresh or [q]uit"))

;; Customize

;; Major mode
(defvar dashi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'dashi/util/refresh)
    map)
  "Keymap for `dashi-mode`.")

(define-derived-mode dashi-mode special-mode "Dashi"
  "Major mode for dashboards."
  (read-only-mode 1))


;; Utilities for general use, layout, etc.
(defun dashi/util/bold (str)
  (propertize str 'face '(:weight bold)))

(defun dashi/util/underline (str)
  (propertize str 'face '(:underline t)))

(defmacro dashi/util/with-newline (&rest strs)
  `(concat ,@strs "\n"))


(defun dashi/util/refresh ()
  (interactive)
  (kill-buffer dashi/buffer-name)
  (dashi))

(defun dashi/util/instructions ()
  (concat "Dashi Emacs Dashboard:   [r]efresh or [q]uit"))

;;; Rendering back-ends

;; box -- simplest one. Also the only one atm
(defun dashi/helper/box-prefix (string)
  (replace-regexp-in-string "^" " | " string))

(defun dashi/decor/box (string)
  (concat "  ,----------\n"
	  (dashi/helper/box-prefix string)
	  "\n  `----------\n"))


;; "Widgets" -- defuns for each group of data. Each function should
;; output the text for a data item to be included in the
;; dashboard. Don't bother ending with a newline, one will be added
;; for you.

(defun dashi/widget/emacs ()
  (concat (dashi/util/with-newline 
	   (dashi/util/bold (emacs-uptime)) "  ⇦  "
	   "Emacs uptime")
	  "Kill-ring items ... "
	  (dashi/util/bold (dashi/item/kill-ring-length))))
(defun dashi/widget/buffers ()
  (concat "Non-hidden buffers ... "
	  (dashi/util/bold (dashi/items/legit-buffer-count))))

(defun dashi/widget/time ()
  (concat "Time/date ... " (dashi/util/bold (current-time-string))))

(defun dashi/widget/directories ()
  (concat "Current directory ... " (dashi/util/bold(dashi/item/pwd))))
 

;; "Items" -- defuns for each datum -- widgets can call any number of
;; these
(defun dashi/item/pwd ()
  default-directory)
(defun dashi/item/kill-ring-length ()
  (int-to-string (length kill-ring)))
(defun dashi/helper/legit-buffer-count ()
  (length (remq nil (mapcar
		     (lambda (buffer-name)
		       (if (not (string-match "^ " buffer-name)) buffer-name))
		     (mapcar 'buffer-name (buffer-list))))))
(defun dashi/items/legit-buffer-count ()
  (int-to-string (dashi/helper/legit-buffer-count)))



;; Main
(defun dashi/decorate-widget (fn widget)
  (funcall fn widget))
 
(defun dashi/make-widget (widget)
  (concat (dashi/decorate-widget
	   dashi/decorator-function
	   (concat (nth 2 widget) " "
		   (dashi/util/underline (nth 1 widget))
		   "\n"
		   (funcall (nth 0 widget))))))

(defun dashi/util/overwrite (str)
  (let ((len (length str)))
    (delete-char len t))
  (insert str))

(defun dashi ()
 (interactive)
  (let ((buf (get-buffer-create dashi/buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq fill-column (window-total-width))
 	(forward-line 1)
	(dashi/util/with-newline (insert dashi/title))
	(newline)
	(beginning-of-buffer)
	(center-line 2)
	(let* ((nlines (window-body-height))
	       (ncols (window-body-width))
;	       (blank-line (make-string ncols ?\s)))
	       (blank-line (make-string ncols ?\.)))
	  (dotimes (_ (- nlines 3))
	    (insert blank-line)
	    (newline)
	    (forward-line 1)))
	(beginning-of-buffer)
	(forward-line 3)
	(dolist (w dashi/widget-list)
	  (dashi/util/overwrite (dashi/make-widget w)))
	(message (dashi/util/instructions))
	(pop-to-buffer-same-window buf)
	(dashi-mode)
	(setq buffer-read-only t)))))

(provide 'dashi)
;;; dashi.el ends here

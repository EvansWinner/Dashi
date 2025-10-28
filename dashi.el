;;; dashi.el --- Dashboards for Emacs   -*- lexical-binding: t; -*-
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
;; Sections todo: files, memory, timers, global modes,
;; functions/symbols

;;; Vars
(defvar dashi/lambda (propertize "λ" 'face '(:weight bold)))


;;; Custom
(defgroup dashi nil
  "Customization options for the Dashi Emacs dashboard system."
  :group 'tools)

(defcustom dashi/default-render-widget-function #'dashi/render/unicode-box
  "The default function used to render a Dashi widget."
  :group 'dashi
  :type '(function :tag "Widget Rendering Function")
  :safe #'symbolp)

(defcustom dashi/default-render-item-function #'dashi/render/item-basic
  "The default function used to render a Dashi 'datum' or atomic item within a widget."
  :group 'dashi
  :type '(function :tag "Item Rendering Function")
  :safe #'symbolp)

(defcustom dashi/widget-list
  ;; Default value
  `(
    (dashi/widget/emacs "Emacs" "💻" nil)
    (dashi/widget/buffers "Buffers" "📃" nil)
    (dashi/widget/time "Time" "🕑" nil)
    (dashi/widget/directories "Directories" "🔢" nil)
    (dashi/widget/processes "Processes" ,dashi/lambda nil)
    )
  "List of Dashi dashboard widgets to display.
Each element is a list of four items:
1. The widget function symbol (e.g., `dashi/widget/time`).
2. A display label string (e.g., \"Time\").
3. An icon string or character (e.g., \"🕑\").
4. An optional action function symbol or nil (e.g., `nil` or `dashi/lambda`)."
  :group 'dashi
  :type '(repeat 
          (list 
           (function :tag "Widget Function")
           (string :tag "Display Label")
           (string :tag "Icon Character/String")
           (choice :tag "Widget Rendering Function (nil for default)"
                   (const :tag "None" nil)
                   (function))))
  )

(defvar dashi/buffer-name "*dashi*")

(defvar dashi/title
  (concat "📊 "
	  (propertize "Dashi Emacs Dashboard" 'face '(:weight bold :underline t))
	  " 📊\n[r]efresh, [b]ury, or [q]uit"))


;;; Major mode
(defvar dashi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'quit-window)
    (define-key map (kbd "q") #'dashi/util/quit)
    (define-key map (kbd "r") #'dashi/util/refresh)
    (define-key map (kbd "?") #'dashi/util/instructions)
    (define-key map (kbd "h") #'dashi/util/instructions)
    map)
  "Keymap for `dashi-mode`.")

(define-derived-mode dashi-mode special-mode "Dashi"
  "Major mode for Dashi dashboards."
  (read-only-mode 1))


;; Utilities for general use, layout, etc.
(defun dashi/util/bold (str)
  (propertize str 'face '(:weight bold)))

(defun dashi/util/underline (str)
  (propertize str 'face '(:underline t)))

(defmacro dashi/util/box (str)
  (propertize str 'face '(:box t)))

(defmacro dashi/util/with-newline (&rest strs)
  `(concat ,@strs "\n"))

(defun dashi/util/refresh ()
  (interactive)
  (kill-buffer dashi/buffer-name)
  (dashi))

(defun dashi/util/quit ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer))

(defun dashi/util/instructions ()
  (interactive)
  (message
   (concat
    "Dashi Emacs Dashboard:   [r]efresh, [b]ury, or [q]uit")))


;;; Rendering back-ends for widgets
(defun dashi/helper/box-prefix (string prefix)
  (replace-regexp-in-string "^" prefix string))

;; ascii-box -- simplest one, should work on TTY
(defun dashi/render/ascii-box (string)
  (concat " .------------------------------\n"
	  (dashi/helper/box-prefix string " | ")
	  "\n `------------------------------\n"))

;; unicode-box -- the default atm
(defun dashi/render/unicode-box (string)
  (concat " ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
	  (dashi/helper/box-prefix string " ┃ ")
	  "\n ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))


;;; Rendering back-ends for items 


;; "Widgets" -- defuns for each group of data. Each function should
;; output the text for an enclosing structure into which one or more
;; "items" will be put.  Don't bother ending with a newline, one will
;; be added for you.

(defun dashi/widget/emacs ()
  (concat (dashi/util/with-newline 
	   "Emacs uptime     ⇨ "
	   (dashi/util/bold (emacs-uptime "%dd %hh %mm %ss")))
	  "Features         ⇨ "
	  (dashi/util/with-newline (dashi/util/bold (dashi/item/features)))
	  "Global modes     ⇨ "
	  (dashi/util/with-newline (dashi/util/bold (dashi/item/global-modes-cnt)))
	  "Kill-ring items  ⇨ "
	  (dashi/util/bold (dashi/item/kill-ring-length))))

(defun dashi/widget/buffers ()
  (concat
   "All            ⇨ "
   (dashi/util/with-newline (dashi/item/all-buffer-count))
   "Non-hidden     ⇨ "
   (dashi/util/with-newline (dashi/util/bold (dashi/item/legit-buffer-count)))
   "Visiting files ⇨ "
   (dashi/util/bold (dashi/item/file-buffer-count))
   ))

(defun dashi/widget/time ()
  (concat "Time/date ⇨ "
	  (dashi/util/bold
	   (format-time-string "%Y-%m-%d %H:%M:%S %Z" (current-time)))))

(defun dashi/widget/directories ()
  (concat "Current directory ⇨ " (dashi/util/bold(dashi/item/pwd))))

(defun dashi/widget/processes ()
  (concat "Emacs sub-processes ⇨ " (dashi/util/bold (dashi/item/processes))))


;; "Items" -- defuns for each datum -- widgets can call any number of
;; these
(defun dashi/item/pwd ()
  (abbreviate-file-name default-directory))
(defun dashi/item/kill-ring-length ()
  (int-to-string (length kill-ring)))
(defun dashi/helper/legit-buffer-count ()
  (int-to-string
   (length (remq nil (mapcar
		      (lambda (buffer-name)
			(if (not (string-match "^ " buffer-name)) buffer-name))
		      (mapcar 'buffer-name (buffer-list)))))))
(defun dashi/item/legit-buffer-count ()
  (dashi/helper/legit-buffer-count))

(defun dashi/item/all-buffer-count ()
  (int-to-string (length (buffer-list))))
(defun dashi/item/file-buffer-count ()
  (int-to-string
   (length
    (remove nil(mapcar #'buffer-file-name (buffer-list))))))

(defun dashi/item/global-modes-cnt ()
  (int-to-string (length global-minor-modes)))

(defun dashi/item/processes ()
  (int-to-string (length (process-list))))

(defun dashi/item/features ()
  (int-to-string (length features)))

;; Main
(defun dashi/render-widget (fn widget)
  (funcall fn widget))
 
(defun dashi/make-widget (widget)
  (concat (dashi/render-widget
	   (or (nth 3 widget) dashi/default-render-widget-function)
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
	(newline)
	(dashi/util/with-newline (insert dashi/title))
	(newline)
	(beginning-of-buffer)
	(center-line 3)
	(forward-line 1)
	(dolist (w dashi/widget-list)
	  (insert (dashi/make-widget w)))
	(center-line)
	(pop-to-buffer-same-window buf)
	(goto-char (point-min))
	(dashi-mode)
	(setq line-spacing 0)
	(dashi/util/instructions)
	(setq buffer-read-only t)))))

(provide 'dashi)
;;; dashi.el ends here

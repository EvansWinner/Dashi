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
;; "widgets" containing interesting facts.  Write your own functions
;; and add them to dashi/widget-list and otherwise modify
;; dabbshi/widget-list to customize the output.

;; "Widgets" (groups of one or more peices of interesting information)
;; are just displayed in the buffer from top to bottom, in the order
;; in which they appear in dashi/widget-list.

;; Items are the actual functions that return the interesting
;; information.  Widgets should call items.  A widget can call as many
;; items as you want.
 
;;; Code:
;; Sections todo: files, timers, functions/symbols

;;; Vars
(defvar dashi/lambda (propertize "λ" 'face '(:weight bold)))
(defvar dashi/pencil (propertize "✎" 'face '(:weight bold)))
(defvar dashi/buffer-name "*dashi*")
(defvar dashi/title
  (concat "📊 "
	  (propertize "Dashi Emacs Dashboard" 'face '(:weight bold :underline t))
	  " 📊\n"))

;;; Custom
(defgroup dashi nil
  "Customization options for the Dashi Emacs dashboard system."
  :group 'tools)

(defcustom dashi/datum-color "red"
  "Color for data items."
  :type 'string
  :group 'dashi)

(defcustom dashi/leftpad "          "
  "String to pad widgets on the left.
Might be used by any number of widget-rendering backends.
Probably spaces.  Could be something else if you want."
  :type 'string
  :group 'dashi)

(defcustom dashi/default-render-widget-function #'dashi/render/unicode-box
  "The default function used to render a Dashi widget."
   :group 'dashi
  :type '(function :tag "Widget Rendering Function")
  :safe #'symbolp)

(defcustom dashi/default-render-item-function #'dashi/render/item-basic
  "Default function used to render a Dashi 'datum'."
  :group 'dashi
  :type '(function :tag "Item Rendering Function")
  :safe #'symbolp)

(defcustom dashi/widget-list
  `(
    (dashi/widget/emacs "Emacs" ,dashi/pencil nil)
    (dashi/widget/system "System" "💻" nil)
    (dashi/widget/buffers "Buffers" "📃" nil)
    (dashi/widget/time "Time" "🕑" nil)
    (dashi/widget/directories "Directories" "🔢" nil)
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


;;; Major mode
(defvar dashi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'quit-window)
    (define-key map (kbd "q") #'dashi/util/quit)
    (define-key map (kbd "r") #'dashi/util/refresh)
    (define-key map (kbd "?") #'dashi/util/instructions)
    (define-key map (kbd "h") #'dashi/util/help)
    (define-key map (kbd " ") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    "Keymap for `dashi-mode`."
    map))

(define-derived-mode dashi-mode special-mode "Dashi"
  "Major mode for Dashi dashboards."
  (font-lock-mode -1)
  (read-only-mode 1))


;; Utilities for general use, layout, etc.
(defun dashi/util/help ()
  (interactive)
  (find-file (concat (file-name-directory (cdr (find-function-library 'dashi))) "README.org")))

(defun dashi/util/bold (str)
  (propertize str 'face '(:weight bold)))

(defun dashi/util/underline (str)
  (propertize str 'face '(:underline t)))

(defun dashi/util/box (str)
  (propertize str 'face '(:box t)))

(defun dashi/util/format-datum (str)
  (propertize (dashi/util/bold str) 'face '(:foreground dashi/datum-color)))
  
(defmacro dashi/util/with-newline (&rest strs)
  `(concat ,@strs "\n"))

(defun dashi/util/refresh ()
  (interactive)
  (kill-buffer dashi/buffer-name)
  (dashi))

(defun dashi/util/leftpad ()
  (dotimes (_ dashi/leftpad) (insert " ")))

(defun dashi/util/quit ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer))

(defun dashi/util/instructions ()
  (interactive)
  (message
   (concat
    "[h]elp   [r]efresh   [b]ury   [q]uit   [DEL]scroll-down   [SPC]scroll-up")))


;;; Rendering back-ends for widgets
(defun dashi/helper/box-prefix (string prefix)
  (replace-regexp-in-string "^" prefix string))

;; minimal -- nothing, really
(defun dashi/render/minimal (string)
  (concat string dashi/leftpad "\n\n"))

;; ascii-box -- simplest one, should work on TTY
(defun dashi/render/ascii-box (string)
  (concat  dashi/leftpad ".------------------------------\n"
	   (dashi/helper/box-prefix string (concat dashi/leftpad "| "))
	   "\n" dashi/leftpad "`------------------------------\n"))

;; unicode-box -- the default atm
(defun dashi/render/unicode-box (string)
  (concat  dashi/leftpad "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
	   (dashi/helper/box-prefix string (concat dashi/leftpad "┃ "))
	 "\n" dashi/leftpad "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))


;;; Rendering back-ends for items
;; An item rendering function takes a function which will return a
;; string (the actual data generator) then a string title.


;; "Widgets" -- defuns for each group of data. Each function should
;; output the text for an enclosing structure into which one or more
;; "items" will be put.  Don't bother ending with a newline, one will
;; be added for you.

(defun dashi/widget/emacs ()
  (concat (dashi/util/with-newline
	   "Emacs uptime       ⇨  "
	   (dashi/util/format-datum (emacs-uptime "%dd %hh %mm %ss")))
	  "Features           ⇨  "
	  (dashi/util/with-newline (dashi/util/format-datum (dashi/item/features)))
	  "Global modes       ⇨  "
	  (dashi/util/with-newline (dashi/util/bold (dashi/item/global-modes-cnt)))
	  "Kill-ring items    ⇨  "
	  (dashi/util/with-newline (dashi/util/bold (dashi/item/kill-ring-length)))
	  "Subprocesses       ⇨  " (dashi/util/bold (dashi/item/processes))))

(defun dashi/widget/system ()
  (concat "Mnemory used        ⇨  "
	   (dashi/util/bold (dashi/item/system-memory))))

(defun dashi/widget/buffers ()
  (concat
   "All                ⇨  "
   (dashi/util/with-newline (dashi/item/all-buffer-count))
   "Non-hidden         ⇨  "
   (dashi/util/with-newline (dashi/util/bold (dashi/item/legit-buffer-count)))
   "Visiting files     ⇨  "
   (dashi/util/bold (dashi/item/file-buffer-count))
   ))

(defun dashi/widget/time ()
  (concat "Time/date ⇨ "
	  (dashi/util/bold
	   (format-time-string "%Y-%m-%d %H:%M:%S %Z" (current-time)))))

(defun dashi/widget/directories ()
  (concat "Current directory  ⇨  " (dashi/util/bold(dashi/item/pwd))))


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

(defun dashi/item/system-memory ()
  (let ((mem (memory-info)))
    (concat (int-to-string (round (* 100 (/ (float (cadr mem)) (float (car mem)))))) "%")))


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
	(goto-char (point-max))
	(kill-line 0)
        ;;magic number 8 gets it more or less centered
	(dotimes (_ (- (/ (window-total-width) 2) 8)) (insert " "))
	(insert-image
	 (create-image
	  (concat (file-name-directory
		   (cdr (find-function-library 'dashi))) "images/dashi.png")
	  nil nil :scale 0.2))
	(dashi-mode)
	(setq line-spacing 0)
	(pop-to-buffer-same-window buf)
	(goto-char (point-min))
	(dashi/util/instructions)
	(setq-local cursor-type nil)
	(setq buffer-read-only t)))))

(provide 'dashi)
;;; dashi.el ends here

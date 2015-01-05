;;; xterm-clip --- Emacs Suppurt tor XTerm Clipboard and Selection

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Olaf Rogalsky <olaf.rogalsky@gmail.com>
;; Keywords: convenience, tools
;; Created: 2015-01-05
;; Version: 1.0

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

;;; Credits:

;; This mode is an adoption of Leo Liu's xclip.el. I've also stolen
;; code from stock emacs select.el and mouse.el.

;;; Commentary:

;; This package provides support of primary and clipboard selection
;; when used in a xterm TTY frame. The protocoll for accessing the
;; selections from xterm does not support the econdory selection.

;; Code

;;; Utility functions

(defun xterm-clip--selection-type-char (type)
  (cond
   ((eq type 'clipboard) ?c)
   ((eq type 'CLIPBOARD) ?c)
   ((eq type 'primary) ?p)
   ((eq type 'PRIMARY) ?p)
   ((eq type 'selection) ?s)
   ((eq type 'SELECTION) ?s)
   ((eq type 'CUT-BUFFER-0) ?0)
   ((eq type 'cut-buffer-0) ?0)
   ((eq type 'CUT-BUFFER-1) ?1)
   ((eq type 'cut-buffer-1) ?1)
   ((eq type 'CUT-BUFFER-2) ?2)
   ((eq type 'cut-buffer-2) ?2)
   ((eq type 'CUT-BUFFER-3) ?3)
   ((eq type 'cut-buffer-3) ?3)
   ((eq type 'CUT-BUFFER-4) ?4)
   ((eq type 'cut-buffer-4) ?4)
   ((eq type 'CUT-BUFFER-5) ?5)
   ((eq type 'cut-buffer-5) ?5)
   ((eq type 'CUT-BUFFER-6) ?6)
   ((eq type 'cut-buffer-6) ?6)
   ((eq type 'CUT-BUFFER-7) ?7)
   ((eq type 'cut-buffer-7) ?7)
   (t (error "Invalid selection type %s" type))))

(defmacro xterm-clip--read-char-and-push (char ls)
  `(progn
     (push (read-char nil nil 0.3) ,ls)
     (unless (eq (car ,ls) ,char)
       (throw 'no-match nil))))

(defun xterm-clip--xterm-bug-workaround (str)
  (substring str 0 (* (/ (length str) 4) 4)))


;;; replacements for corresponding functions in select.el

;; TODO: integrate into select.el (or do an advice ???)
(defun xterm-clip-set-selection (type data)
  "TYPE is nil or a symbol: primary (same as nil), secondary and clipboard.

See `gui-set-selection'."
  (cond
   ((eq (framep (selected-frame)) t)
    (send-string-to-terminal
     (concat
      "\e]52;"
      (list (xterm-clip--selection-type-char type))
      ";"
      (base64-encode-string (encode-coding-string data 'binary) t)
      "\e\\")))
   (t
    (gui-set-selection type data))))

;; TODO: integrate into select.el (or do an advice to gui-get-selection ???)
(defun xterm-clip-get-selection (type)
  "TYPE is nil or a symbol: primary (same as nil), secondary and clipboard.

See `gui-set-selection'."
  (cond
   ((eq (framep (selected-frame)) t)
    (send-string-to-terminal
     (concat
      "\e]52;"
      (list (xterm-clip--selection-type-char type))
      ";?\e\\"))
    (let ((response nil)
          (data nil))
      ;; if xterm's response does not match the excepted string, we replay the response
      (or
       (catch 'no-match
         (xterm-clip--read-char-and-push ?\e response)
         (xterm-clip--read-char-and-push ?\] response)
         (xterm-clip--read-char-and-push ?5 response)
         (xterm-clip--read-char-and-push ?2 response)
         (xterm-clip--read-char-and-push ?\; response)
         (xterm-clip--read-char-and-push (xterm-clip--selection-type-char type) response)
         (xterm-clip--read-char-and-push ?\; response)
         (while (progn
                  (push (read-char) data)
                  (push (car data) response)
                  (not (eq (car data) ?\e))))
         (xterm-clip--read-char-and-push ?\\ response)
         (decode-coding-string (base64-decode-string (xterm-clip--xterm-bug-workaround
                                                      (concat (reverse (cdr data)))))
                               'utf-8-unix))
       (dolist (c response "")
         (push c unread-command-events)))))
   (t
    (gui-get-selection type))))

;; TODO: integrate into select.el (or do an advice to gui-select-text ???)
(defun xterm-clip-select-text (text)
  "Select TEXT, a string. This function is only usefull when used
within a xterm frame.
If `select-enable-clipboard' is non-nil, copy TEXT to the system's clipboard.
If `select-enable-primary' is non-nil, put TEXT in the primary selection.
See also `gui-select-text'."
  (cond
   ((eq (framep (selected-frame)) t)
    (when select-enable-primary
      (xterm-clip-set-selection 'PRIMARY text)
      (setq gui--last-selected-text-primary text))
    (when select-enable-clipboard
      ;; When cutting, the selection is cleared and PRIMARY
      ;; set to the empty string.  Prevent that, PRIMARY
      ;; should not be reset by cut (Bug#16382).
      (setq saved-region-selection text)
      (xterm-clip-set-selection 'CLIPBOARD text)
      (setq gui--last-selected-text-clipboard text)))
   (t
    (gui-select-text text))))

;; TODO: integrate into select.el (or do an advice to gui-selection-value ???)
(defun xterm-clip-selection-value ()
  (cond
   ((eq (framep (selected-frame)) t)
    (let ((clip-text
           (when select-enable-clipboard
             (let ((text (xterm-clip-get-selection 'clipboard)))
               (if (string= text "") (setq text nil))

               ;; Check the CLIPBOARD selection for 'newness', is it different
               ;; from what we remembered them to be last time we did a
               ;; cut/paste operation.
               (prog1
                   (unless (equal text gui--last-selected-text-clipboard)
                     text)
                 (setq gui--last-selected-text-clipboard text)))))
          (primary-text
           (when select-enable-primary
             (let ((text (xterm-clip-get-selection 'PRIMARY)))
               (if (string= text "") (setq text nil))
               ;; Check the PRIMARY selection for 'newness', is it different
               ;; from what we remembered them to be last time we did a
               ;; cut/paste operation.
               (prog1
                   (unless (equal text gui--last-selected-text-primary)
                     text)
                 (setq gui--last-selected-text-primary text))))))
      ;; NOTE: There will be cases where more than one of these has
      ;; changed and the new values differ.  This indicates that
      ;; something like the following has happened since the last time
      ;; we looked at the selections: Application X set all the
      ;; selections, then Application Y set only one of them.
      ;; In this case since we don't have
      ;; timestamps there is no way to know what the 'correct' value to
      ;; return is.  The nice thing to do would be to tell the user we
      ;; saw multiple possible selections and ask the user which was the
      ;; one they wanted.
      (or clip-text primary-text)))
   (t
    (gui-selection-value))))

;; adoption of mouse-yank-primary for xterm selection mechanism
;; TODO: integrate into select.el
(defun xterm-clip--mouse-yank-primary (click)
  (run-hooks 'mouse-leave-buffer-hook)
  ;; Without this, confusing things happen upon e.g. inserting into
  ;; the middle of an active region.
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (or mouse-yank-at-point (mouse-set-point click))
  (let ((primary (xterm-clip-get-selection 'primary)))
    (push-mark (point))
    (insert-for-yank primary)))

(defun xterm-clip--mouse-yank-primary-advice (mouse-yank-primary-fun &rest click)
  (if (eq (framep (selected-frame)) t)
      (apply #'xterm-clip--mouse-yank-primary click)
    (apply mouse-yank-primary-fun click)))

;; Mouse-set-region does not set primary -- this is done in the toolkit.
;; We therefore advice mouse-set-region to set primary.
;; TODO: Advising is suboptimal: better include the code into mouse-set-region.
(defun xterm-clip--mouse-set-region-advice (click)
  (if (eq (framep (selected-frame)) t)
      (xterm-clip-set-selection
       'primary
       (buffer-substring-no-properties (region-beginning) (region-end)))))


;;; minor mode definitions

(defun xterm-clip--enable ()
  (advice-add 'mouse-yank-primary :around #'xterm-clip--mouse-yank-primary-advice)
  (advice-add 'mouse-set-region :after #'xterm-clip--mouse-set-region-advice)
  (setq interprogram-cut-function 'xterm-clip-select-text)
  (setq interprogram-paste-function 'xterm-clip-selection-value))

(defun xterm-clip--disable ()
  (advice-remove 'mouse-yank-primary #'xterm-clip--mouse-yank-primary-advice)
  (advice-remove 'mouse-set-region #'xterm-clip--mouse-set-region-advice)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

;;;###autoload
(define-minor-mode xterm-clip-mode
  "Minor mode to use the `xterm-clip' program to copy&paste."
  :global t
  (cond
   (xterm-clip-mode
    (add-hook 'terminal-init-xterm-hook 'xterm-clip--enable)
    (xterm-clip--enable))
   (t
    (remove-hook 'terminal-init-xterm-hook 'xterm-clip--enable)
    (xterm-clip--disable))))

(provide 'xterm-clip)
;;; xterm-clip.el ends here

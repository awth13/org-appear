;;; org-togglers.el --- Auto-toggle Org fragments -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-togglers Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode emphasis markers upon entering and leaving an emphasised fragment
;; Homepage: https://github.com/awth13/org-togglers

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package enabes automatic toggling of various Org fragments depending on cursor position.

;;; Code:

(require 'org)
(require 'org-element)

;; TODO: Manual toggling
;; (defcustom org-togglers-manual nil
;;   "Non-nil means that automatic toggling is disabled."
;;   :type 'boolean
;;   :group 'org)
;;
;; (defun org-togglers-at-point ()
;;   "Toggle fragment at point."
;;   (interactive)
;;   (let ((current-frag (org-togglers--current-frag)))
;;     (when current-frag
;;       (org-togglers--toggle current-frag))))

;; TODO: Custom settings per-type

;;;###autoload
(define-minor-mode org-togglers-mode
  "A minor mode that automatically toggles fragments in Org mode.
Markers are shown when the cursor enters an emphasised fragment and hidden when the
cursor leaves the fragment."
  nil nil nil

  (cond
   (org-togglers-mode
    (add-hook 'post-command-hook #'org-togglers--post-cmd nil t))
   (t
    (let ((current-frag (org-togglers--current-frag)))
      (when current-frag
	(org-togglers--disable current-frag)))
    (remove-hook 'post-command-hook #'org-togglers--post-cmd t))))

(defvar-local org-togglers--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-togglers--post-cmd ()
  "This function is executed by `post-command-hook' in `org-togglers-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
  (let* ((prev-frag org-togglers--prev-frag)
	 (prev-frag-start (car prev-frag))
	 (current-frag (org-togglers--current-frag))
	 (current-frag-start (car current-frag)))
    ;; Do nothing if fragment did not change
    (when (not (equal prev-frag-start current-frag-start))
      ;; Current fragment is the new previous
      (setq org-togglers--prev-frag current-frag)
      ;; Hide markers in previous fragment, if any
      (when prev-frag
	(org-togglers--disable prev-frag))
      ;; Show markers in current fragment, if any
      (when current-frag
	(org-togglers--enable current-frag)))))

(defun org-togglers--current-frag ()
  "Return start position, end position, and type of a fragment if cursor is inside one."
  (let* ((elem (org-element-context))
	 (elem-type (car elem))
	 (elem-start (org-element-property :begin elem))
	 (elem-end (org-element-property :end elem)))
    (cond ((member elem-type '(bold
			       italic
			       underline
			       strike-through))
	   (list elem-start elem-end 'emph))
	  ((member elem-type '(verbatim
			       code))
	   (list elem-start elem-end 'verbatim))
	  ((member elem-type '(latex-fragment
			       latex-environment))
	   (list elem-start elem-end 'latex))
	  ((equal elem-type 'link)
	   (list elem-start elem-end 'link))
	  (t nil))))

(defun org-togglers--frag-pos (frag)
  "Return positions of fragment FRAG."
  (let ((start (car frag))
	(end (cadr frag))
	(type (caddr frag)))
    (cond ((equal type 'link)
	   (save-excursion
	     (goto-char start)
	     (when (looking-at org-link-any-re)
	       (let ((start (match-beginning 0))
		     (end (match-end 0))
		     (visible-start (or (match-beginning 3) (match-beginning 2)))
		     (visible-end (or (match-end 3) (match-end 2))))
		 (list start end visible-start visible-end)))))
	  ((equal type 'latex)
	   (list start end))
	  (t
	   (save-excursion
	     (goto-char (1- start))
	     (when (looking-at (if (equal type 'verbatim) org-verbatim-re org-emph-re))
	       (let* ((start (match-beginning 2))
		      (end (match-end 2))
		      (visible-start (1+ start))
		      (visible-end (1- end)))
		 (list start end visible-start visible-end))))))))

(defun org-togglers--enable (frag)
  "Enable visibility of fragment FRAG."
  (let ((type (caddr frag)))
    (cond ((equal type 'latex)
	   (org-togglers--hide-latex-preview (org-togglers--frag-pos frag)))
	  (t
	   (org-togglers--show-invisible (org-togglers--frag-pos frag))))))

(defun org-togglers--disable (frag)
  "Disable visibility of fragment FRAG."
  (let ((type (caddr frag)))
    (cond ((equal type 'latex)
	   (org-togglers--show-latex-preview (org-togglers--frag-pos frag)))
	  (t
	   (org-togglers--hide-invisible (org-togglers--frag-pos frag))))))

;;; Emphasis and Links
(defun org-togglers--show-invisible (pos)
  "Silently remove invisible property from text at POS."
  (let ((start (nth 0 pos))
	(end (nth 1 pos))
	(visible-start (nth 2 pos))
	(visible-end (nth 3 pos)))
    (catch 'passed-nil
      (if (eq start nil)
	  (throw 'passed-nil nil)
	(with-silent-modifications
	  (remove-text-properties start visible-start '(invisible nil))
	  (remove-text-properties visible-end end '(invisible nil)))))))

(defun org-togglers--hide-invisible (pos)
  "Silently add invisible property to text at POS."
  (let ((start (nth 0 pos))
	(end (nth 1 pos))
	(visible-start (nth 2 pos))
	(visible-end (nth 3 pos)))
    ;; If an emphasis marker is deleted when the cursor is inside an emphasised fragment,
    ;; `org-togglers--hide-markers' is called with nil as an argument
    ;; TODO: a more robust fix
    (catch 'passed-nil
      (if (eq start nil)
	  (throw 'passed-nil nil)
	(with-silent-modifications
	  (put-text-property start visible-start 'invisible t)
	  (put-text-property visible-end end 'invisible t))))))

;;; LaTeX Fragments
(defun org-togglers--show-latex-preview (pos)
  "Enable preview of the LaTeX fragment at POS."

  ;; The fragment must be disabled before `org-latex-preview', since
  ;; `org-latex-preview' only toggles, leaving no guarantee that it's enabled
  ;; afterwards.
  (org-togglers--hide-latex-preview pos)

  (save-excursion
    (goto-char (car pos))
    (org-latex-preview)))

(defun org-togglers--hide-latex-preview (pos)
  "Disable preview of the LaTeX fragment at POS."
  (org-clear-latex-preview (car pos) (cadr pos)))

(provide 'org-togglers)
;;; org-togglers.el ends here

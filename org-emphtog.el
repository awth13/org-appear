;;; org-emphtog.el --- Auto-toggle Org emphasis markers -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-emphtog Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode emphasis markers upon entering and leaving an emphasised fragment
;; Homepage: https://github.com/awth13/org-emphtog

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

;; This package enabes automatic toggling of Org emphasis markers depending on cursor position.
;; Emphasis (verbatim) markers are shown when the cursor enters an emphasised fragment
;; and hidden when the cursor leaves the fragment.

;;; Code:

(require 'org)
(require 'org-element)

;;;###autoload
(define-minor-mode org-emphtog-mode
  "A minor mode that automatically toggles visibility of emphasis markers in Org mode.
Markers are shown when the cursor enters an emphasised fragment and hidden when the
cursor leaves the fragment. If `org-hide-emphasis-markers' is nil, the mode does not load."
  nil nil nil

  (when org-hide-emphasis-markers
    (cond
     (org-emphtog-mode
      (add-hook 'post-command-hook #'org-emphtog--post-cmd nil t))
     ;; When disabled, clean up by hiding markers around current fragment, if any
     (t
      (let ((current-frag (org-emphtog--current-frag)))
	(when current-frag
	  (org-emphtog--hide-markers (org-emphtog--get-position current-frag))))
      (remove-hook 'post-command-hook #'org-emphtog--post-cmd t)))))

(defvar-local org-emphtog--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-emphtog--post-cmd ()
  "This function is executed by `post-command-hook' in `org-emphtog-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
  (let* ((prev-frag org-emphtog--prev-frag)
	 (prev-frag-start (car prev-frag))
	 (current-frag (org-emphtog--current-frag))
	 (current-frag-start (car current-frag)))
    ;; Do nothing if fragment did not change
    (when (not (equal prev-frag-start current-frag-start))
      ;; Current fragment is the new previous
      (setq org-emphtog--prev-frag current-frag)
      ;; Hide markers in previous fragment, if any
      (when prev-frag
	(org-emphtog--hide-markers (org-emphtog--get-position prev-frag)))
      ;; Show markers in current fragment, if any
      (when current-frag
	(org-emphtog--show-markers (org-emphtog--get-position current-frag))))))

(defun org-emphtog--current-frag ()
  "Return the start position and type of emphasised fragment if cursor is inside one."
  (let* ((elem (org-element-context))
	 (elem-type (car elem))
	 (elem-start (org-element-property :begin elem)))
    (if (member elem-type '(bold
			    italic
			    underline
			    strike-through
			    verbatim
			    code))
	(cons elem-start elem-type)
      nil)))

(defun org-emphtog--get-position (frag)
  "Return consed positions of opening and closing emphasis markers of an emphasised fragment FRAG."
  (let ((start (car frag))
	(verbatim? (member (cdr frag) '(verbatim code))))
    (save-excursion
      (goto-char (1- start))
      (looking-at (if verbatim? org-verbatim-re org-emph-re))
      (cons (match-beginning 2) (match-end 2)))))

(defun org-emphtog--show-markers (match)
  "Silently remove invisible property from markers at MATCH."
  (with-silent-modifications
    (let ((start (car match))
	  (end (cdr match)))
      (remove-text-properties start (1+ start) '(invisible org-link))
      (remove-text-properties (1- end) end '(invisible org-link)))))

(defun org-emphtog--hide-markers (match)
  "Silently add invisible property to markers at MATCH."
  (with-silent-modifications
    (let ((start (car match))
	  (end (cdr match)))
      (put-text-property start (1+ start) 'invisible 'org-link)
      (put-text-property (1- end) end 'invisible 'org-link))))

(provide 'org-emphtog)
;;; org-emphtog.el ends here

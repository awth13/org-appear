;;; org-appear.el --- Auto-toggle Org fragments -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode fragment visibility upon entering and leaving
;; Homepage: https://github.com/awth13/org-appear

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

(defgroup org-appear nil
  "Auto-toggle Org fragments"
  :group 'org)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim fragments.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript markers.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolinks nil
  "Non-nil enables automatic toggling of links.
Does not have an effect if `org-link-descriptive' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolatex nil
  "Non-nil enables automatic toggling of LaTeX fragments."
  :type 'boolean
  :group 'org-appear)

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles fragments in Org mode."
  nil nil nil

  (cond
   (org-appear-mode
    (org-appear--set-fragments)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t))
   (t
    (let ((current-frag (org-appear--current-frag)))
      (when current-frag
	(org-appear--disable current-frag)))
    (remove-hook 'post-command-hook #'org-appear--post-cmd t))))

(defvar org-appear-fragments nil
  "List of Org fragments to toggle.")

(defun org-appear--set-fragments ()
  "Add designated fragments to toggle to `org-appear-fragments'."
  (let ((emph-fragments '(bold
			  italic
			  underline
			  strike-through
			  verbatim
			  code))
	(subscript-fragments '(subscript
			       superscript))
	(latex-fragments '(latex-fragment
			   latex-environment))
	(link-fragments '(link)))
    (setq org-appear-fragments nil)
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-fragments (append org-appear-fragments emph-fragments)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-fragments (append org-appear-fragments subscript-fragments)))
    (when org-appear-autolatex
      (setq org-appear-fragments (append org-appear-fragments latex-fragments)))
    (when (and org-link-descriptive org-appear-autolinks)
      (setq org-appear-fragments (append org-appear-fragments link-fragments)))))

(defvar-local org-appear--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
  (let* ((prev-frag org-appear--prev-frag)
	 (prev-frag-start (org-element-property :begin prev-frag))
	 (current-frag (org-appear--current-frag))
	 (current-frag-start (org-element-property :begin current-frag)))
    ;; Do nothing if fragment did not change
    (when (not (equal prev-frag-start current-frag-start))
      ;; Current fragment is the new previous
      (setq org-appear--prev-frag current-frag)
      ;; Hide markers in previous fragment, if any
      (when prev-frag
	(org-appear--disable prev-frag))
      ;; Show markers in current fragment, if any
      (when current-frag
	(org-appear--enable current-frag)))))

(defun org-appear--current-frag ()
  "Return element list of fragment at point.
Return nil if element is not supported by `org-appear-mode'."
  (let ((elem (org-element-context)))
    (if (member (car elem) org-appear-fragments)
	elem
      nil)))

(defun org-appear--enabled-p (frag)
  "Return non-nil if fragment FRAG is enabled."
  (not (get-text-property (org-element-property :begin frag) 'invisible)))

(defun org-appear-toggle-at-point ()
  "Toggle fragment at point."
  (interactive)
  (let ((current-frag (org-appear--current-frag)))
    (pcase (car current-frag)
      ((or 'latex-fragment 'latex-environment)
       (org-latex-preview))
      (type
       (if (org-appear--enabled-p current-frag)
	   (org-appear--disable current-frag)
	 (org-appear--enable current-frag))))))

(defun org-appear--parse-elem (elem)
  "Parse element ELEM.
TODO: Extracted info."
  (let* ((elem-type (car elem))
	 (elem-start (org-element-property :begin elem))
	 (elem-end (org-element-property :end elem))
	 (elem-content-start (org-element-property :contents-begin elem))
	 (elem-content-end (org-element-property :contents-end elem))
	 (post-elem-spaces (org-element-property :post-blank elem))
	 (elem-end-real (- elem-end post-elem-spaces)))
    (cond ((member elem-type '(bold
			       italic
			       underline
			       strike-through
			       verbatim
			       code))
	   (list 'start elem-start
		 'end elem-end-real
		 'visible-start (1+ elem-start)
		 'visible-end (1- elem-end-real)
		 'type 'emph))
	  ((member elem-type '(subscript
			       superscript))
	   (list 'start elem-start
		 'end elem-end-real
		 'visible-start elem-content-start
		 'visible-end elem-content-end
		 'type 'subscript))
	  ((member elem-type '(latex-fragment
			       latex-environmet))
	   (list 'start elem-start
		 'end elem-end-real
		 'type 'latex))
	  ((equal elem-type 'link)
	   (let ((visible-start (org-element-property :contents-begin elem))
		 (visible-end (org-element-property :contents-end elem)))
	     (list 'start elem-start
		   'end elem-end-real
		   'visible-start (if visible-start
				      visible-start
				    (+ elem-start 2))
		   'visible-end (if visible-end
				    visible-end
				  (- elem-end-real 2))
		   'type 'link)))
	  (t nil))))

(defun org-appear--enable (elem)
  "Enable visibility of element ELEM."
  (let ((frag-props (org-appear--parse-elem elem)))
    (pcase (plist-get frag-props 'type)
      ('latex
       (org-appear--hide-latex-preview frag-props))
      ((or 'emph 'link 'subscript)
       (org-appear--show-invisible frag-props)))))

(defun org-appear--disable (elem)
  "Disable visibility of element ELEM."
  (let ((frag-props (org-appear--parse-elem elem)))
    (pcase (plist-get frag-props 'type)
      ('latex
       (org-appear--show-latex-preview frag-props))
      ((or 'emph 'link 'subscript)
       (org-appear--hide-invisible frag-props)))))

;;; Emphasis
(defun org-appear--show-invisible (frag)
  "Silently remove invisible property inside fragment FRAG."
  (let ((start (plist-get frag 'start))
	(end (plist-get frag 'end))
	(visible-start (plist-get frag 'visible-start))
	(visible-end (plist-get frag 'visible-end)))
    (catch 'passed-nil
      (if (eq start nil)
	  (throw 'passed-nil nil)
	(with-silent-modifications
	  (remove-text-properties start visible-start '(invisible nil))
	  (remove-text-properties visible-end end '(invisible nil)))))))

(defun org-appear--hide-invisible (frag)
  "Silently add invisible property to inside fragment FRAG."
  (let ((start (plist-get frag 'start))
	(end (plist-get frag 'end))
	(visible-start (plist-get frag 'visible-start))
	(visible-end (plist-get frag 'visible-end)))
    ;; If an emphasis marker is deleted when the cursor is inside an emphasised fragment,
    ;; `org-appear--hide-markers' is called with nil as an argument
    ;; TODO: a more robust fix
    (catch 'passed-nil
      (if (eq start nil)
	  (throw 'passed-nil nil)
	(with-silent-modifications
	  (put-text-property start visible-start 'invisible t)
	  (put-text-property visible-end end 'invisible t))))))

;;; LaTeX Fragments
(defun org-appear--show-latex-preview (frag)
  "Enable preview of the LaTeX fragment FRAG."

  ;; The fragment must be disabled before `org-latex-preview', since
  ;; `org-latex-preview' only toggles, leaving no guarantee that it's enabled
  ;; afterwards.
  (org-appear--hide-latex-preview frag)

  (save-excursion
    (goto-char (plist-get frag 'start))
    (org-latex-preview)))

(defun org-appear--hide-latex-preview (frag)
  "Disable preview of the LaTeX fragment FRAG."
  (org-clear-latex-preview (plist-get frag 'start)
			   (plist-get frag 'end)))

(provide 'org-appear)
;;; org-appear.el ends here

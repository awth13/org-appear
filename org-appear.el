;;; org-appear.el --- Auto-toggle Org elements -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Version: 0.0.3
;; Description: Toggle Org mode element visibility upon entering and leaving
;; Homepage: https://github.com/awth13/org-appear
;; Package-Requires: ((emacs "25.1") (org "9.3"))

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

;; This package enables automatic visibility toggling of various Org elements depending on cursor position.
;; Automatic toggling of elements may be enabled by setting `org-appear-autoemphasis',
;; `org-appear-autolinks', and `org-appear-autosubmarkers' custom variables to non-nil.
;; By default, only `org-appear-autoemphasis' is enabled.
;; If Org mode custom variables that control visibility of emphasis markers, links,
;; or sub/superscripts are configured to show hidden parts,
;; the respective `org-appear' settings do not have an effect.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-appear nil
  "Auto-toggle Org elements"
  :group 'org)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim markers.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript markers.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autoentities nil
  "Non-nil enables automatic toggling of org entities.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolinks nil
  "Non-nil enables automatic toggling of links.
Does not have an effect if `org-link-descriptive' is nil."
  :type 'boolean
  :group 'org-appear)

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles elements in Org mode."
  :init-value nil
  :lighter nil
  :keymap nil

  (cond
   (org-appear-mode
    (org-appear--set-elements)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t)
    (add-hook 'pre-command-hook #'org-appear--pre-cmd nil t))
   (t
    ;; Clean up current element when disabling the mode
    (let ((current-elem (org-appear--current-elem)))
      (when current-elem
	(org-appear--hide-invisible current-elem)))
    (remove-hook 'post-command-hook #'org-appear--post-cmd t)
    (remove-hook 'pre-command-hook #'org-appear--pre-cmd t))))

(defvar org-appear-elements nil
  "List of Org elements to toggle.")

(defvar-local org-appear--prev-elem nil
  "Previous element that surrounded the cursor or nil if the cursor was not
on an element.")

(defun org-appear--set-elements ()
  "Add elements to toggle to `org-appear-elements'."
  (let ((emphasis-elements '(bold
			     italic
			     underline
			     strike-through
			     verbatim
			     code))
	(script-elements '(subscript
			   superscript))
	(entity-elements '(entity))
	(link-elements '(link)))

    ;; HACK: is there a better way to do this?
    (setq-local org-appear--prev-elem nil)
    (setq org-appear-elements nil)	; reset
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-elements (append org-appear-elements emphasis-elements)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-elements (append org-appear-elements script-elements)))
    (when (and org-pretty-entities org-appear-autoentities)
      (setq org-appear-elements (append org-appear-elements entity-elements)))
    (when (and org-link-descriptive org-appear-autolinks)
      (setq org-appear-elements (append org-appear-elements link-elements)))))

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling elements depending on whether the cursor entered or exited them."
  (let* ((prev-elem org-appear--prev-elem)
	 (prev-elem-start (org-element-property :begin prev-elem))
	 (current-elem (org-appear--current-elem))
	 (current-elem-start (org-element-property :begin current-elem))
	 (current-elem-end (org-element-property :end current-elem)))

    ;; Hide invisible parts of previous element if cursor left it
    (when (and prev-elem
	       (not (equal prev-elem-start current-elem-start)))
      (save-excursion
	(goto-char prev-elem-start)
	;; Reevaluate `org-element-context' in case the bounds
	;; of the previous element changed
	(org-appear--hide-invisible (org-element-context))
	;; Forget previous element
	(setq org-appear--prev-elem nil)))

    ;; Unhide invisible parts of current element after each command
    (when current-elem
      ;; Remember current element as the last visited element
      (setq org-appear--prev-elem current-elem)
      ;; Call `font-lock-ensure' before unhiding to prevent `jit-lock-mode'
      ;; from refontifying the element region after changes in buffer
      (font-lock-ensure current-elem-start current-elem-end)
      (org-appear--show-invisible current-elem))))

(defun org-appear--pre-cmd ()
  "This function is executed by `pre-command-hook' in `org-appear-mode'.
It hides elements before commands that modify the buffer based on column width."
  (when (memq this-command '(org-fill-paragraph
			     org-ctrl-c-ctrl-c))
    (when-let ((current-elem (org-appear--current-elem)))
      (org-appear--hide-invisible current-elem))))

(defun org-appear--current-elem ()
  "Return element at point.
Return nil if element is not supported by `org-appear-mode'."
  (when-let ((elem (org-element-context)))
    (let ((elem-type (car elem))
	  (elem-end (- (org-element-property :end elem)
		       (1- (org-element-property :post-blank elem))))
	  (elem-ignorep (string= (org-element-property :type elem) "cite")))
      (if (and (memq elem-type org-appear-elements)
	       (< (point) elem-end)     ; Ignore post-element whitespace
	       (not elem-ignorep))      ; Ignore specific elements
	  elem
	nil))))

(defun org-appear--get-parent (elem)
  "Return parent element if ELEM is nested inside another valid element."
  (let ((parent (org-element-property :parent elem)))
    (when (memq (car parent) '(bold
			       italic
			       underline
			       strike-through
			       verbatim
			       code
			       subscript
			       superscript))
      parent)))

(defun org-appear--parse-elem (elem)
  "Return bounds of element ELEM and its parent if ELEM is nested.
Return nil if element cannot be parsed."
  (let* ((elem-type (car elem))
	 (link-subtype (org-element-property :type elem))
	 (elem-tag (cond ((memq elem-type '(bold
					    italic
					    underline
					    strike-through
					    verbatim
					    code))
			  'emph)
			 ((memq elem-type '(subscript
					    superscript))
			  'script)
			 ((eq elem-type 'entity)
			  'entity)
			 ((eq elem-type 'link)
			  'link)
			 (t nil)))
	 (elem-start (org-element-property :begin elem))
	 (elem-end (org-element-property :end elem))
	 (elem-content-start (org-element-property :contents-begin elem))
	 (elem-content-end (org-element-property :contents-end elem))
	 ;; Some elements have extra spaces at the end
	 ;; The number of spaces is stored in the post-blank property
	 (post-elem-spaces (org-element-property :post-blank elem))
	 (elem-end-real (- elem-end post-elem-spaces))
	 (elem-parent (org-appear--get-parent elem)))
    ;; Only sub/superscript elements are guaranteed to have
    ;; contents-begin and contents-end properties
    (when elem-tag
      `(:start ,elem-start
	       :end ,elem-end-real
	       :visible-start ,(pcase elem-tag
				 ('emph (1+ elem-start))
				 ('script elem-content-start)
				 ('link (or elem-content-start (+ elem-start 2))))
	       :visible-end ,(pcase elem-tag
			       ('emph (1- elem-end-real))
			       ('script elem-content-end)
			       ('link (or elem-content-end (- elem-end-real 2))))
	       :parent ,elem-parent))))

(defun org-appear--show-invisible (elem)
  "Silently remove invisible property from invisible parts of element ELEM."
  (let* ((elem-at-point (org-appear--parse-elem elem))
	 (elem-type (car elem))
	 (start (plist-get elem-at-point :start))
	 (end (plist-get elem-at-point :end))
	 (visible-start (plist-get elem-at-point :visible-start))
	 (visible-end (plist-get elem-at-point :visible-end))
	 (parent (plist-get elem-at-point :parent)))
    (with-silent-modifications
      (if (eq elem-type 'entity)
	  (remove-text-properties start end '(composition))
	(remove-text-properties start visible-start '(invisible org-link))
	(remove-text-properties visible-end end '(invisible org-link))))
    ;; To minimise distraction from moving text,
    ;; always keep parent emphasis markers visible
    (when parent
      (org-appear--show-invisible parent))))

(defun org-appear--hide-invisible (elem)
  "Flush fontification of element ELEM."
  (let* ((elem-at-point (org-appear--parse-elem elem))
	 (elem-type (car elem))
	 (start (plist-get elem-at-point :start))
	 (end (plist-get elem-at-point :end)))
    (font-lock-flush start end)
    ;; Call `font-lock-ensure' after flushing to prevent `jit-lock-mode'
    ;; from refontifying the next element entered
    (font-lock-ensure start end)
    (when (eq elem-type 'entity)
      (goto-char start))))

(provide 'org-appear)
;;; org-appear.el ends here

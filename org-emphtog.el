;; Original work Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; Emphasis marker modification Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode emphasis markers upon entering and leaving an emphasised fragment
;; Homepage: https://github.com/io12/org-fragtog

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

(require 'org)

;;;###autoload
(define-minor-mode org-emphtog-mode
  "A minor mode that automatically toggles visibility of emphasis markers in Org mode.
Markers are shown when the cursor enters an emphasised fragment and hidden when the
cursor leaves the fragment. If 'org-hide-emphasis-markers' is nil, the mode does nothing."
  nil nil nil

  (cond
   (org-emphtog-mode
    (add-hook 'post-command-hook #'org-emphtog--post-cmd nil t))
   ;; When disabled, clean up by hiding markers around current fragment, if any
   (t
    (let ((current-frag (org-emphtog--current-frag)))
      (when current-frag
	(org-emphtog--hide-markers current-frag)))
    (remove-hook 'post-command-hook #'org-emphtog--post-cmd t))))

(defvar-local org-emphtog--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-emphtog--post-cmd ()
  "This function is executed by `post-command-hook' in `org-emphtog-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
  ;; Do nothing if markers are not hidden
  (when org-hide-emphasis-markers
    (let* ((prev-frag org-emphtog--prev-frag)
	   (current-frag (org-emphtog--current-frag)))

      ;; Do nothing if fragment did not change
      (when (not (equal prev-frag current-frag))
	;; Current fragment is the new previous
	(setq org-emphtog--prev-frag current-frag)
	;; Hide markers in previous fragment, if any
	(when prev-frag
	  (org-emphtog--hide-markers prev-frag))
	;; Show markers in current fragment, if any
	(when current-frag
	  (org-emphtog--show-markers current-frag))))))

(defun org-emphtog--current-frag ()
  "Return a cons cell with locations of emphasis markers if cursor is inside
an emphasised fragment, else return nil."
  ;; org-in-regexp may add extra chars to match string,
  ;; hence the use of match-beginning/end to get precise marker locations
  (when (or (org-in-regexp org-emph-re 0 t)
	    (org-in-regexp org-verbatim-re 0 t))
    (cons (match-beginning 2)
	  (match-end 2))))

(defun org-emphtog--show-markers (frag)
  "Silently remove invisible property from markers."
  (with-silent-modifications
    (let* ((start (car frag))
	   (end (cdr frag)))
      (remove-text-properties start (1+ start) '(invisible org-link))
      (remove-text-properties (1- end) end '(invisible org-link)))))

(defun org-emphtog--hide-markers (frag)
  "Silently add invisible property to markers"
  (with-silent-modifications
    (let* ((start (car frag))
	   (end (cdr frag)))
      (put-text-property start (1+ start) 'invisible 'org-link)
      (put-text-property (1- end) end 'invisible 'org-link))))

(provide 'org-emphtog)

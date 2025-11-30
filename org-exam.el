;;; org-exam.el --- Org Export Backend for LaTeX Exam Class -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Daniel Muñoz <demunoz2@uc.cl>
;; Keywords: text, convenience, abbrev
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/koprotk/org-exam

;;; Commentary:

;; This package provides support for LaTeX's exam document class in Org export.
;; It works with the standard LaTeX export (C-c C-e l o).
;; 
;; Usage:
;; 1. Add to your init file:
;;    (require 'org-exam)
;;
;; 2. In your Org file, use:
;;    #+LATEX_CLASS: exam
;;
;; 3. Export normally with C-c C-e l o (or l l, l p, etc.)
;;
;; Features:
;; - Questions, parts, and subparts with automatic environment wrapping
;; - Points notation in headlines or properties
;; - Choices (- lists) and checkboxes (+ lists)
;; - @correct marker for correct answers
;; - Solutions using :solution: drawers
;; - Full LaTeX preamble support with babel, packages, etc.

;;; Code:

(require 'ox-latex)

;;; Configuration Variables

(defvar org-exam-current-class nil
  "Buffer-local variable to track if current export uses exam class.")

;;; Setup exam class in org-latex-classes

(defun org-exam-setup-latex-class ()
  "Add exam class to `org-latex-classes' if not already present."
  (unless (assoc "exam" org-latex-classes)
    (add-to-list 'org-latex-classes
                 '("exam"
                   "[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ;; We use custom transcoders, these are placeholders
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;; Run setup when loading
(org-exam-setup-latex-class)

;;; Helper Functions

(defun org-exam-get-points (headline)
  "Extract points from HEADLINE properties or title."
  (or (org-element-property :POINTS headline)
      (let ((title (org-element-property :raw-value headline)))
        (when (string-match "\\[\\([0-9.]+\\)\\s-*\\(?:pts?\\|points?\\)?\\]" title)
          (match-string 1 title)))))

(defun org-exam-clean-title (title)
  "Remove points notation from TITLE."
  (replace-regexp-in-string "\\[\\([0-9.]+\\)\\s-*\\(?:pts?\\|points?\\)?\\]" "" title))

(defun org-exam-headline-level-type (level)
  "Determine exam element type based on headline LEVEL.
1 = question, 2 = part, 3 = subpart"
  (cond
   ((= level 1) 'question)
   ((= level 2) 'part)
   ((= level 3) 'subpart)
   (t 'question)))

(defun org-exam-get-direct-children (headline)
  "Get direct children headlines of HEADLINE."
  (let ((level (org-element-property :level headline))
        (children '()))
    (org-element-map (org-element-contents headline) 'headline
      (lambda (child)
        (when (= (org-element-property :level child) (1+ level))
          (push child children)))
      nil nil 'headline)
    (nreverse children)))

(defun org-exam-is-exam-class-p (info)
  "Check if current export uses exam document class.
INFO is the plist with export information."
  (let ((latex-class (plist-get info :latex-class)))
    (and latex-class (string= latex-class "exam"))))

;;; Transcode Functions for Exam Class

(defun org-exam-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX exam format.
CONTENTS is the contents of the headline. INFO is a plist holding
contextual information."
  (if (not (org-exam-is-exam-class-p info))
      ;; Not exam class - use default LaTeX export
      (org-latex-headline headline contents info)
    ;; Exam class - use custom export
    (let* ((level (org-element-property :level headline))
           (type (org-exam-headline-level-type level))
           (raw-title (org-element-property :raw-value headline))
           (title (org-exam-clean-title raw-title))
           (points (org-exam-get-points headline)))
      
      (cond
       ;; Question level
       ((eq type 'question)
        (let* ((children (org-exam-get-direct-children headline))
               (text-content (org-exam-get-text-content headline info))
               (parts-content (when children
                                (mapconcat
                                 (lambda (child)
                                   (org-exam-transcode-part child info))
                                 children
                                 ""))))
          (concat
           (if points
               (format "\\question[%s] " points)
             "\\question ")
           title "\n"
           (when text-content
             (concat text-content "\n"))
           (when parts-content
             (concat "\\begin{parts}\n"
                     parts-content
                     "\\end{parts}\n"))
           "\n")))
       
       ;; For parts and subparts, we handle them separately
       (t "")))))

(defun org-exam-get-text-content (headline info)
  "Get text content of HEADLINE excluding child headlines."
  (let ((section (org-element-map (org-element-contents headline) 'section
                   #'identity nil t)))
    (when section
      (org-export-data section info))))

(defun org-exam-transcode-part (part info)
  "Transcode a PART headline to LaTeX."
  (let* ((raw-title (org-element-property :raw-value part))
         (title (org-exam-clean-title raw-title))
         (points (org-exam-get-points part))
         (children (org-exam-get-direct-children part))
         (text-content (org-exam-get-text-content part info))
         (subparts-content (when children
                             (mapconcat
                              (lambda (child)
                                (org-exam-transcode-subpart child info))
                              children
                              ""))))
    (concat
     (if points
         (format "\\part[%s] " points)
       "\\part ")
     title "\n"
     (when text-content
       (concat text-content "\n"))
     (when subparts-content
       (concat "\\begin{subparts}\n"
               subparts-content
               "\\end{subparts}\n"))
     "\n")))

(defun org-exam-transcode-subpart (subpart info)
  "Transcode a SUBPART headline to LaTeX."
  (let* ((raw-title (org-element-property :raw-value subpart))
         (title (org-exam-clean-title raw-title))
         (points (org-exam-get-points subpart))
         (text-content (org-exam-get-text-content subpart info)))
    (concat
     (if points
         (format "\\subpart[%s] " points)
       "\\subpart ")
     title "\n"
     (when text-content
       (concat text-content "\n"))
     "\n")))

;;; Plain List Functions

(defun org-exam-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
  (if (not (org-exam-is-exam-class-p info))
      ;; Not exam class - use default LaTeX export
      (org-latex-plain-list plain-list contents info)
    ;; Exam class - check for choices/checkboxes
    (let* ((type (org-element-property :type plain-list))
           (first-item (org-element-map plain-list 'item #'identity info t))
           (first-bullet (when first-item
                           (org-element-property :bullet first-item))))
      (cond
       ;; Unordered list starting with + becomes checkboxes
       ((and (eq type 'unordered)
             first-bullet
             (string-prefix-p "+" first-bullet))
        (concat "\\begin{checkboxes}\n"
                (org-exam-process-list-items plain-list info)
                "\\end{checkboxes}\n"))
       ;; Unordered list starting with - becomes choices
       ((and (eq type 'unordered)
             first-bullet
             (string-prefix-p "-" first-bullet))
        (concat "\\begin{choices}\n"
                (org-exam-process-list-items plain-list info)
                "\\end{choices}\n"))
       ;; Default case - use standard latex export
       (t (org-latex-plain-list plain-list contents info))))))

(defun org-exam-process-list-items (plain-list info)
  "Process items in PLAIN-LIST for choices/checkboxes.
INFO is the plist with export information."
  (mapconcat
   (lambda (item)
     (org-exam-process-choice-item item info))
   (org-element-map plain-list 'item #'identity info)
   ""))

(defun org-exam-process-choice-item (item info)
  "Process a single ITEM for choices/checkboxes.
INFO is the plist with export information."
  (let* ((paragraph (org-element-map (org-element-contents item) 'paragraph
                      #'identity info t))
         (text (when paragraph
                 (org-export-data (org-element-contents paragraph) info)))
         (has-correct (and text (string-match-p "@correct" text)))
         (clean-text (if text
                         (replace-regexp-in-string "@correct[[:space:]]*" "" text)
                       ""))
         (choice-cmd (if has-correct "\\CorrectChoice" "\\choice")))
    (concat choice-cmd " " clean-text "\n")))

(defun org-exam-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS is the contents of the item. INFO is a plist holding
contextual information."
  (if (not (org-exam-is-exam-class-p info))
      ;; Not exam class - use default LaTeX export
      (org-latex-item item contents info)
    ;; Exam class - check if this is a choices/checkboxes list
    (let* ((plain-list (org-export-get-parent item))
           (type (org-element-property :type plain-list))
           (bullet (org-element-property :bullet item)))
      (if (and (eq type 'unordered)
               bullet
               (or (string-prefix-p "+" bullet)
                   (string-prefix-p "-" bullet)))
          ;; Don't process here - handled by org-exam-plain-list
          ""
        ;; Default case - use standard latex export
        (org-latex-item item contents info)))))

;;; Drawer Functions

(defun org-exam-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS is the contents of the drawer. INFO is a plist holding
contextual information."
  (if (not (org-exam-is-exam-class-p info))
      ;; Not exam class - use default LaTeX export
      (org-latex-drawer drawer contents info)
    ;; Exam class - check for solution drawer
    (let ((name (org-element-property :drawer-name drawer)))
      (cond
       ;; Solution drawer becomes \begin{solution}...\end{solution}
       ((string-match-p "^solution$" (downcase name))
        (concat "\\begin{solution}\n"
                contents
                "\\end{solution}\n"))
       ;; Other drawers - use default export
       (t (org-latex-drawer drawer contents info))))))

;;; Template Function

(defun org-exam-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (if (not (org-exam-is-exam-class-p info))
      ;; Not exam class - use default LaTeX template
      (org-latex-template contents info)
    ;; Exam class - use custom template with questions environment
    (let* ((latex-class (plist-get info :latex-class))
           (class-options (let ((opts (plist-get info :latex-class-options)))
                            (cond
                             ;; If it's a string, clean it up
                             ((stringp opts)
                              (setq opts (org-trim opts))
                              ;; Remove brackets if user included them
                              (when (string-prefix-p "[" opts)
                                (setq opts (substring opts 1)))
                              (when (string-suffix-p "]" opts)
                                (setq opts (substring opts 0 -1)))
                              (unless (string-empty-p opts) opts))
                             ;; Otherwise nil
                             (t nil))))
           (title (org-export-data (plist-get info :title) info))
           (author (org-export-data (plist-get info :author) info))
           (date (org-export-data (plist-get info :date) info))
           (language (plist-get info :language))
           ;; Use org-latex-make-preamble but without documentclass
           (preamble (org-latex-make-preamble info))
           ;; Add babel if language is specified
           (babel-package (when language
                            (format "\\usepackage[%s]{babel}\n" 
                                    (org-exam-get-babel-language language))))
           (latex-header (mapconcat 'identity
                                    (org-element-map (plist-get info :parse-tree) 'keyword
                                      (lambda (k)
                                        (when (string= (org-element-property :key k) "LATEX_HEADER")
                                          (org-element-property :value k))))
                                    "\n"))
           (latex-header-extra (mapconcat 'identity
                                          (org-element-map (plist-get info :parse-tree) 'keyword
                                            (lambda (k)
                                              (when (string= (org-element-property :key k) "LATEX_HEADER_EXTRA")
                                                (org-element-property :value k))))
                                          "\n")))
      (concat
       ;; Document class
       "\\documentclass"
       (when class-options (format "[%s]" class-options))
       "{" latex-class "}\n"
       
       ;; Preamble (packages) - properly generated by org-latex
       preamble
       
       ;; Babel for language support
       (when babel-package babel-package)
       
       ;; User's latex headers
       (when (and latex-header (not (string-empty-p latex-header)))
         (concat latex-header "\n"))
       (when (and latex-header-extra (not (string-empty-p latex-header-extra)))
         (concat latex-header-extra "\n"))
       
       "\n"
       "\\begin{document}\n"
       
       ;; Title, author, date
       (when title (concat "\\title{" title "}\n"))
       (when author (concat "\\author{" author "}\n"))
       (when date (concat "\\date{" date "}\n"))
       (when (or title author date) "\\maketitle\n\n")
       
       ;; Main content in questions environment
       "\\begin{questions}\n"
       contents
       "\\end{questions}\n"
       "\\end{document}\n"))))

(defun org-exam-get-babel-language (language)
  "Convert org LANGUAGE to babel language option."
  (cond
   ((string= language "es") "spanish")
   ((string= language "en") "english")
   ((string= language "fr") "french")
   ((string= language "de") "german")
   ((string= language "it") "italian")
   ((string= language "pt") "portuguese")
   (t language)))

;;; Register Filter Functions

(defun org-exam-latex-headline-filter (headline backend info)
  "Filter function for headlines.
HEADLINE is the transcoded headline string.
BACKEND is the export backend.
INFO is the plist with export options."
  ;; Only apply custom headline processing for exam class
  (if (and (org-export-derived-backend-p backend 'latex)
           (org-exam-is-exam-class-p info))
      (org-exam-headline 
       (get-text-property 0 'org-element headline)
       headline
       info)
    headline))

;;; Advice Functions to Override Transcoders

(defun org-exam-override-latex-transcoders ()
  "Add advice to LaTeX export functions to support exam class."
  ;; Override headline transcoder
  (advice-add 'org-latex-headline :around #'org-exam-headline-advice)
  ;; Override plain-list transcoder
  (advice-add 'org-latex-plain-list :around #'org-exam-plain-list-advice)
  ;; Override item transcoder
  (advice-add 'org-latex-item :around #'org-exam-item-advice)
  ;; Override drawer transcoder
  (advice-add 'org-latex-drawer :around #'org-exam-drawer-advice)
  ;; Override template
  (advice-add 'org-latex-template :around #'org-exam-template-advice))

(defun org-exam-headline-advice (orig-fun headline contents info)
  "Advice for org-latex-headline to support exam class.
ORIG-FUN is the original function.
HEADLINE, CONTENTS, INFO are the standard arguments."
  (if (org-exam-is-exam-class-p info)
      (org-exam-headline headline contents info)
    (funcall orig-fun headline contents info)))

(defun org-exam-plain-list-advice (orig-fun plain-list contents info)
  "Advice for org-latex-plain-list to support exam class.
ORIG-FUN is the original function.
PLAIN-LIST, CONTENTS, INFO are the standard arguments."
  (if (org-exam-is-exam-class-p info)
      (org-exam-plain-list plain-list contents info)
    (funcall orig-fun plain-list contents info)))

(defun org-exam-item-advice (orig-fun item contents info)
  "Advice for org-latex-item to support exam class.
ORIG-FUN is the original function.
ITEM, CONTENTS, INFO are the standard arguments."
  (if (org-exam-is-exam-class-p info)
      (org-exam-item item contents info)
    (funcall orig-fun item contents info)))

(defun org-exam-drawer-advice (orig-fun drawer contents info)
  "Advice for org-latex-drawer to support exam class.
ORIG-FUN is the original function.
DRAWER, CONTENTS, INFO are the standard arguments."
  (if (org-exam-is-exam-class-p info)
      (org-exam-drawer drawer contents info)
    (funcall orig-fun drawer contents info)))

(defun org-exam-template-advice (orig-fun contents info)
  "Advice for org-latex-template to support exam class.
ORIG-FUN is the original function.
CONTENTS, INFO are the standard arguments."
  (if (org-exam-is-exam-class-p info)
      (org-exam-template contents info)
    (funcall orig-fun contents info)))

;;; Activation

;; Automatically apply advice when package is loaded
(org-exam-override-latex-transcoders)

(provide 'org-exam)
;;; org-exam.el ends here

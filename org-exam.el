;;; ox-exam.el --- Org Export Backend for LaTeX Exam Class -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Daniel Muñoz
;; Keywords: text, convenience, abbrev
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/koprotk/org-exam

;;; Commentary:

;; This package provides an Org export backend for LaTeX's exam document class.
;; It supports:
;; - Questions with points
;; - Parts and subparts environments
;; - Automatic environment wrapping
;; - Choices and checkboxes with @correct marker
;;
;; Usage:
;; Export with: C-c C-e E o

;;; Code:

(require 'ox-latex)

;;; Setup exam class in org-latex-classes

(defun org-exam-setup-latex-class ()
  "Add exam class to `org-latex-classes' if not already present."
  (unless (assoc "exam" org-latex-classes)
    (add-to-list 'org-latex-classes
                 '("exam"
                   "\\documentclass{exam}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;; Run setup when loading
(org-exam-setup-latex-class)

;;; Define Exam Backend

(org-export-define-derived-backend 'exam 'latex
  :menu-entry
  '(?E "Export to Exam LaTeX"
    ((?L "As LaTeX buffer" org-exam-export-as-latex)
     (?l "As LaTeX file" org-exam-export-to-latex)
     (?p "As PDF file" org-exam-export-to-pdf)
     (?o "As PDF file and open" org-exam-export-to-pdf-and-open)))
  :options-alist
  '((:exam-class "EXAM_CLASS" nil "exam" t)
    (:exam-header "EXAM_HEADER" nil nil newline)
    (:latex-class-options "LATEX_CLASS_OPTIONS" nil nil t)
    (:language "LANGUAGE" nil "en" t))
  :translate-alist
  '((headline . org-exam-headline)
    (section . org-exam-section)
    (plain-list . org-exam-plain-list)
    (item . org-exam-item)
    (template . org-exam-template)))

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

(defun org-exam-has-direct-children (headline)
  "Check if HEADLINE has direct children headlines."
  (org-element-map (org-element-contents headline) 'headline
    (lambda (child)
      (= (org-element-property :level child)
         (1+ (org-element-property :level headline))))
    nil t 'headline))

;;; Template

(defun org-exam-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (let* ((class (plist-get info :exam-class))
         (class-options (plist-get info :latex-class-options))
         (header (plist-get info :exam-header))
         (title (org-export-data (plist-get info :title) info))
         (author (org-export-data (plist-get info :author) info))
         (date (org-export-data (plist-get info :date) info))
         (language (plist-get info :language))
         (latex-header (plist-get info :latex-header))
         (latex-header-extra (plist-get info :latex-header-extra)))
    (concat
     ;; Document class
     "\\documentclass"
     (when class-options (format "[%s]" class-options))
     "{" class "}\n"
     
     ;; Default packages (similar to org-latex default)
     (org-exam-get-default-packages info)
     
     ;; Babel for language support
     (when language
       (format "\\usepackage[%s]{babel}\n" 
               (org-exam-get-babel-language language)))
     
     ;; User's latex headers
     (when latex-header (concat latex-header "\n"))
     (when latex-header-extra (concat latex-header-extra "\n"))
     (when header (concat header "\n"))
     
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
     "\\end{document}\n")))

(defun org-exam-get-default-packages (info)
  "Get default LaTeX packages for exam export.
INFO is the plist with export options."
  (concat
   ;; Input encoding
   "\\usepackage[utf8]{inputenc}\n"
   "\\usepackage[T1]{fontenc}\n"
   
   ;; Graphics
   "\\usepackage{graphicx}\n"
   "\\usepackage{grffile}\n"
   
   ;; Math
   "\\usepackage{amsmath}\n"
   "\\usepackage{amssymb}\n"
   
   ;; Links and colors
   "\\usepackage{xcolor}\n"
   "\\usepackage{hyperref}\n"
   "\\hypersetup{\n"
   " colorlinks=true,\n"
   " linkcolor=blue,\n"
   " filecolor=magenta,\n"
   " urlcolor=cyan,\n"
   " pdftitle={" (org-export-data (plist-get info :title) info) "},\n"
   " pdfauthor={" (org-export-data (plist-get info :author) info) "}\n"
   "}\n"
   
   ;; Formatting
   "\\usepackage{longtable}\n"
   "\\usepackage{float}\n"
   "\\usepackage{wrapfig}\n"
   "\\usepackage{rotating}\n"
   "\\usepackage{textcomp}\n"
   "\\usepackage{capt-of}\n"))

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

;;; Section Transcoder

(defun org-exam-section (section contents info)
  "Transcode a SECTION element from Org to LaTeX.
CONTENTS is the section contents. INFO is a plist holding
contextual information."
  ;; Return contents as-is; we handle everything in headline transcoder
  contents)

;;; Plain List Transcoder

(defun org-exam-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
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
     (t (org-latex-plain-list plain-list contents info)))))

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
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (bullet (org-element-property :bullet item)))
    ;; Check if this is a choices/checkboxes list
    (if (and (eq type 'unordered)
             bullet
             (or (string-prefix-p "+" bullet)
                 (string-prefix-p "-" bullet)))
        ;; Don't process here - handled by org-exam-plain-list
        ""
      ;; Default case - use standard latex export
      (org-latex-item item contents info))))

;;; Headline Transcoder

(defun org-exam-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX exam format.
CONTENTS is the contents of the headline. INFO is a plist holding
contextual information."
  (let* ((level (org-element-property :level headline))
         (type (org-exam-headline-level-type level))
         (raw-title (org-element-property :raw-value headline))
         (title (org-exam-clean-title raw-title))
         (points (org-exam-get-points headline))
         (has-children (org-exam-has-direct-children headline)))
    
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
     (t ""))))

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

;;; Export Functions

(defun org-exam-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as an Exam LaTeX buffer.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, overriding the region.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only return body
code, without the document wrapper.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org EXAM Export*\"."
  (interactive)
  (org-export-to-buffer 'exam "*Org EXAM Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

(defun org-exam-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Exam LaTeX file.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, overriding the region.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only return body
code, without the document wrapper.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'exam outfile
      async subtreep visible-only body-only ext-plist)))

(defun org-exam-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Exam PDF file.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, overriding the region.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only return body
code, without the document wrapper.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'exam outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(defun org-exam-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Exam PDF file and open it.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, overriding the region.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only return body
code, without the document wrapper.
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'exam outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-open-file (org-latex-compile file))))))

(provide 'ox-exam)
;;; ox-exam.el ends here

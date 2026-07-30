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

;; This package provides a derived Org export backend for LaTeX's exam
;; document class.  It inherits from the standard `latex' backend and
;; overrides only the parts specific to exam documents.
;;
;; Usage:
;;   1. (require 'org-exam)
;;   2. Use M-x org-exam-export-to-latex (or org-exam-export-as-latex)
;;   3. Optionally set #+LATEX_CLASS: exam for the correct document class
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
(require 'cl-lib)

;;; Customisation

(defgroup org-exam nil
  "LaTeX exam document class exporter for Org mode."
  :group 'org-export
  :prefix "org-exam-")

(defcustom org-exam-point-name " pts"
  "Replacement text for the exam class `\\pointname' macro.
The leading space is conventional so points render as `5 pts'.  Use a
short, language-neutral abbreviation to keep exports portable across
languages.  Set to nil to keep the exam class default (` point' /
` points').  May be overridden per-document with `#+EXAM_POINT_NAME:'."
  :type '(choice (string :tag "Replacement text")
                 (const :tag "Use exam class default" nil))
  :group 'org-exam)

(defcustom org-exam-default-footer-center "\\thepage"
  "Default centre footer applied when a custom header is in use.
Switching to the `headandfoot' pagestyle (which `EXAM_HEADER_*' does)
disables the bare page number that the plain pagestyle prints at the
bottom of every page.  This value restores it.  Set to nil to leave the
centre footer empty when only a header is provided."
  :type '(choice (string :tag "LaTeX expression")
                 (const :tag "No default" nil))
  :group 'org-exam)

(defcustom org-exam-header-image-height "1.5cm"
  "Default LaTeX `height' for images placed in header/footer slots.
Used when an `EXAM_HEADER_*_IMAGE' or `EXAM_FOOTER_*_IMAGE' keyword
provides no `:height' or `:width' attribute.  Any LaTeX length is
accepted (e.g. `2cm', `20mm', `0.8in')."
  :type 'string
  :group 'org-exam)

;;; Setup exam class in org-latex-classes

(defun org-exam-setup-latex-class ()
  "Add exam class to `org-latex-classes' if not already present."
  (unless (assoc "exam" org-latex-classes)
    (add-to-list 'org-latex-classes
                 '("exam"
                   "\\documentclass[11pt]{exam}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;;; Backend definition

(org-export-define-derived-backend 'exam 'latex
  :translate-alist '((headline . org-exam-headline)
                     (plain-list . org-exam-plain-list)
                     (item . org-exam-item)
                     (drawer . org-exam-drawer)
                     (template . org-exam-template))
  :options-alist '((:exam-header-left "EXAM_HEADER_LEFT" nil nil)
                   (:exam-header-center "EXAM_HEADER_CENTER" nil nil)
                   (:exam-header-right "EXAM_HEADER_RIGHT" nil nil)
                   (:exam-footer-left "EXAM_FOOTER_LEFT" nil nil)
                   (:exam-footer-center "EXAM_FOOTER_CENTER" nil nil)
                   (:exam-footer-right "EXAM_FOOTER_RIGHT" nil nil)
                   (:exam-point-name "EXAM_POINT_NAME" nil nil)))

;;; Export commands

;;;###autoload
(defun org-exam-export-as-latex (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX exam buffer."
  (interactive)
  (org-export-to-buffer 'exam "*Org Exam Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-exam-export-to-latex (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX exam file."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'exam outfile
      async subtreep visible-only body-only ext-plist)))

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
1 = question, 2 = part, 3 = subpart.  Levels deeper than 3 return nil
because the LaTeX exam class has no nesting beyond subparts."
  (cond
   ((= level 1) 'question)
   ((= level 2) 'part)
   ((= level 3) 'subpart)
   (t nil)))

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

;;; Transcode Functions

(defun org-exam-headline (headline _contents info)
  "Transcode a HEADLINE element from Org to LaTeX exam format.
INFO is a plist holding contextual information."
  (let* ((level (org-element-property :level headline))
         (type (org-exam-headline-level-type level))
         (raw-title (org-element-property :raw-value headline))
         (title (org-exam-clean-title raw-title))
         (points (org-exam-get-points headline)))
    (cond
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
     ((memq type '(part subpart)) "")
     (t
      (display-warning 'org-exam
                       (format "Dropping headline `%s' at level %d (exam class supports up to subparts)"
                               raw-title level)
                       :error)
      ""))))

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
  (let* ((type (org-element-property :type plain-list))
         (first-item (org-element-map plain-list 'item #'identity info t))
         (first-bullet (when first-item
                         (org-element-property :bullet first-item))))
    (cond
     ((and (eq type 'unordered)
           first-bullet
           (string-prefix-p "+" first-bullet))
      (concat "\\begin{checkboxes}\n"
              (org-exam-process-list-items plain-list info)
              "\\end{checkboxes}\n"))
     ((and (eq type 'unordered)
           first-bullet
           (string-prefix-p "-" first-bullet))
      (concat "\\begin{choices}\n"
              (org-exam-process-list-items plain-list info)
              "\\end{choices}\n"))
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
INFO is the plist with export information.  The @correct marker is
recognised only when it appears at the start of the item text so that a
literal occurrence further inside the answer does not get stripped."
  (let* ((paragraph (org-element-map (org-element-contents item) 'paragraph
                      #'identity info t))
         (text (when paragraph
                 (org-trim (org-export-data (org-element-contents paragraph) info))))
         (has-correct (and text (string-match-p "\\`@correct\\(?:[[:space:]]\\|\\'\\)" text)))
         (clean-text (if has-correct
                         (replace-regexp-in-string "\\`@correct[[:space:]]*" "" text)
                       (or text "")))
         (choice-cmd (if has-correct "\\CorrectChoice" "\\choice")))
    (concat choice-cmd " " clean-text "\n")))

(defun org-exam-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS is the contents of the item. INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (bullet (org-element-property :bullet item)))
    (if (and (eq type 'unordered)
             bullet
             (or (string-prefix-p "+" bullet)
                 (string-prefix-p "-" bullet)))
        ""
      (org-latex-item item contents info))))

;;; Drawer Functions

(defun org-exam-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to LaTeX.
CONTENTS is the contents of the drawer. INFO is a plist holding
contextual information."
  (let ((name (org-element-property :drawer-name drawer)))
    (cond
     ((string-match-p "^solution$" (downcase name))
      (concat "\\begin{solution}\n"
              contents
              "\\end{solution}\n"))
     (t (org-latex-drawer drawer contents info)))))

;;; Info plist helpers

(defun org-exam--normalize-class-options (info)
  "Return INFO with `:latex-class-options' bracketed if needed."
  (let* ((opts (plist-get info :latex-class-options))
         (trimmed (and (stringp opts) (org-trim opts))))
    (cond
     ((or (null trimmed) (string-empty-p trimmed)) info)
     ((string-prefix-p "[" trimmed) info)
     (t (org-combine-plists info
                            (list :latex-class-options
                                  (format "[%s]" trimmed)))))))

(defun org-exam--inject-babel (info)
  "Return INFO with babel auto-loaded when missing."
  (let* ((header (or (plist-get info :latex-header) ""))
         (header-extra (or (plist-get info :latex-header-extra) ""))
         (existing (concat header "\n" header-extra)))
    (if (string-match-p "\\\\usepackage[^\n{]*{\\(babel\\|polyglossia\\)}"
                        existing)
        info
      (org-combine-plists info
                          (list :latex-header
                                (concat header
                                        "\n\\usepackage[AUTO]{babel}"))))))

(defun org-exam--image-link-to-latex (value)
  "Convert an Org file link at the start of VALUE to \\includegraphics.
Returns nil when VALUE is not an image link."
  (when (and value
             (string-match
              "\\`[[:space:]]*\\[\\[\\(?:file:\\)?\\([^]\n]+\\)\\(?:\\]\\[[^]\n]*\\)?\\]\\][[:space:]]*\\(.*\\)\\'"
              value))
    (let* ((path (match-string 1 value))
           (attrs (match-string 2 value))
           (height (and (string-match ":height[[:space:]]+\\([^[:space:]]+\\)"
                                      attrs)
                        (match-string 1 attrs)))
           (width (and (string-match ":width[[:space:]]+\\([^[:space:]]+\\)"
                                     attrs)
                       (match-string 1 attrs))))
      (format "\\includegraphics[%s]{%s}"
              (cond
               (width (format "width=%s" width))
               (height (format "height=%s" height))
               (t (format "height=%s" org-exam-header-image-height)))
              path))))

(defun org-exam--slot-value (info slot)
  "Return the LaTeX expression for header/footer SLOT from INFO.
SLOT is the suffix after `EXAM_' (e.g. \"HEADER_LEFT\")."
  (let* ((key (intern (concat ":exam-"
                              (downcase
                               (replace-regexp-in-string "_" "-" slot)))))
         (raw (plist-get info key)))
    (or (org-exam--image-link-to-latex raw) raw "")))

(defun org-exam--header-footer-preamble (info)
  "Build preamble snippets for header, footer, and label overrides.
Returns a string of LaTeX lines (possibly empty)."
  (let* ((hl (org-exam--slot-value info "HEADER_LEFT"))
         (hc (org-exam--slot-value info "HEADER_CENTER"))
         (hr (org-exam--slot-value info "HEADER_RIGHT"))
         (fl (org-exam--slot-value info "FOOTER_LEFT"))
         (fc (org-exam--slot-value info "FOOTER_CENTER"))
         (fr (org-exam--slot-value info "FOOTER_RIGHT"))
         (point-name-kw (plist-get info :exam-point-name))
         (point-name (cond
                       ((and point-name-kw (not (string-blank-p point-name-kw)))
                        (concat " " (org-trim point-name-kw)))
                       (t org-exam-point-name)))
         (has-header (not (and (string-blank-p hl)
                               (string-blank-p hc)
                               (string-blank-p hr))))
         (has-footer (not (and (string-blank-p fl)
                               (string-blank-p fc)
                               (string-blank-p fr))))
         (need-headandfoot (or has-header has-footer))
         (lines nil))
    (when point-name
      (push (format "\\pointname{%s}" point-name) lines))
    (when need-headandfoot
      (push "\\pagestyle{headandfoot}" lines))
    (when has-header
      (push (format "\\header{%s}{%s}{%s}" hl hc hr) lines))
    (cond
     (has-footer
      (push (format "\\footer{%s}{%s}{%s}" fl fc fr) lines))
     ((and need-headandfoot org-exam-default-footer-center)
      (push (format "\\footer{}{%s}{}" org-exam-default-footer-center) lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun org-exam--inject-exam-preamble (info)
  "Append exam-specific preamble customisation to INFO's `:latex-header'."
  (let ((snippet (org-exam--header-footer-preamble info)))
    (if (or (null snippet) (string-empty-p snippet))
        info
      (org-combine-plists info
                          (list :latex-header
                                (concat (or (plist-get info :latex-header) "")
                                        "\n" snippet))))))

(defun org-exam--uses-headandfoot-p (info)
  "Return non-nil when any EXAM_HEADER_* or EXAM_FOOTER_* keyword is set."
  (cl-some (lambda (slot)
             (let ((v (org-exam--slot-value info slot)))
               (and v (not (string-blank-p v)))))
           '("HEADER_LEFT" "HEADER_CENTER" "HEADER_RIGHT"
             "FOOTER_LEFT" "FOOTER_CENTER" "FOOTER_RIGHT")))

(defun org-exam--user-set-option-p (info option)
  "Return non-nil if the user explicitly set OPTION in any #+OPTIONS line."
  (cl-some
   (lambda (k)
     (and (string= (org-element-property :key k) "OPTIONS")
          (string-match-p (format "\\b%s:" (regexp-quote option))
                          (or (org-element-property :value k) ""))))
   (org-element-map (plist-get info :parse-tree) 'keyword #'identity)))

(defun org-exam--suppress-toc (info)
  "Disable the table of contents for exam exports.
A TOC is rarely useful in an exam, so default `:with-toc' to nil unless
the user explicitly set `toc:' in `#+OPTIONS'."
  (if (org-exam--user-set-option-p info "toc")
      info
    (org-combine-plists info '(:with-toc nil))))

(defun org-exam--wrap-questions (contents)
  "Wrap CONTENTS in `\\begin{questions}…\\end{questions}'."
  (if (string-match "^\\\\question\\(?:\\[\\|[ \t\n]\\)" contents)
      (let ((split (match-beginning 0)))
        (concat (substring contents 0 split)
                "\\begin{questions}\n"
                (substring contents split)
                "\\end{questions}\n"))
    contents))

(defun org-exam-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded body string. INFO is a plist holding
export options."
  (let ((body (org-exam--wrap-questions contents))
        (info (org-combine-plists
               info
               (org-exam--suppress-toc
                (org-exam--inject-exam-preamble
                 (org-exam--inject-babel
                  (org-exam--normalize-class-options info)))))))
    (when (org-exam--uses-headandfoot-p info)
      (setq body (concat "\\thispagestyle{headandfoot}\n" body)))
    (org-latex-template body info)))

;;; Activation

(org-exam-setup-latex-class)

(provide 'org-exam)

;;; org-exam.el ends here

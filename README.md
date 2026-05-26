# org-exam

An Emacs package that exports Org documents to LaTeX using the [`exam`](https://www.ctan.org/pkg/exam) document class. It plugs into the standard LaTeX exporter, so you keep using the familiar `C-c C-e l …` workflow and get an `exam`-shaped `.tex` (and PDF) out the other side.

See [`sample-exam.org`](sample-exam.org) for a working example and [`sample-exam.pdf`](sample-exam.pdf) for the rendered output.

# Installation

## Manual installation

1. Clone the repository:

``` sh
git clone https://github.com/koprotk/org-exam.git
```

2. Add the folder to your `load-path` and require it:

``` emacs-lisp
(add-to-list 'load-path "~/path/to/org-exam")
(require 'org-exam)
```

The exporter is now available.

## With use-package and straight.el

``` emacs-lisp
(use-package org-exam
  :straight (org-exam :host github :repo "koprotk/org-exam")
  :defer t)
```

## Doom Emacs

1. In your `packages.el`:

``` emacs-lisp
(package! org-exam
  :recipe (:host github :repo "koprotk/org-exam"))
```

2. After editing `packages.el`:

``` sh
doom sync
```

# Usage

1. Add the exam class to your Org file:

``` org
#+LATEX_CLASS: exam
```

2. Export with the standard LaTeX export commands:

 - `C-c C-e l l` — export to LaTeX
 - `C-c C-e l o` — export to LaTeX and open the resulting PDF

`org-exam` handles a few things for you automatically when `#+LATEX_CLASS: exam` is in effect:

 - generates a single, deduplicated preamble (no double `\usepackage` lines);
 - loads `babel` based on the `#+LANGUAGE` keyword;
 - skips the default `\tableofcontents` (set `#+OPTIONS: toc:t` to bring it back);
 - sets `\pointname` to a language-neutral abbreviation (see [Point label](#point-label));
 - accepts `#+LATEX_CLASS_OPTIONS: answers` either with or without brackets;
 - emits any Org content placed *before* the first `*` headline (instructions, name/section fields, formula sheets, …) above `\begin{questions}` so the exam class accepts it.

# Features supported
## Questions (of course)
Every top-level headline (`*`) becomes a question.
``` org
* 
```

## Parts
Use `**` for parts. A part must live under a question.
``` org
*
**
```

## Subparts
Each part can be broken down into subparts with `***`. A subpart must live under a part.
``` org
*
**
***
```

## Points
Points can be attached to a question, part, or subpart either inline as `[N pts]` or as a property.

``` org
* Question [15 pts]
```

or

``` org
* Question 
:PROPERTIES:
:POINTS: 15
:END:
```

## Choices
Choices are written as unordered lists using `-`. Mark the correct one with `@correct`.

``` org
* Question [1 pts]
- choice 1
- choice 2
- choice 3
- @correct choice 4
```

Choices are printed as A., B., C., etc. The marker style can be overridden via the LaTeX class options.

Correct choices are printed bold only when the document is exported with the `answers` class option:

``` org
#+LATEX_CLASS_OPTIONS: answers
```


## Checkboxes
Same as choices, but use `+` as the list marker.


## Solutions
To include a solution, wrap the answer in a `:solution:` drawer:

``` org
* Question:
:solution:
here goes your answer of the questions
:end:
```

The solution box is printed only when the document is exported with the `answers` class option:
``` org
#+LATEX_CLASS_OPTIONS: answers
```

## Page header and footer
Set a running header and/or footer with up to six keywords. Any one of them switches the document to the `headandfoot` page style and emits the corresponding `\header{}{}{}` / `\footer{}{}{}` command. Values are inserted verbatim, so LaTeX macros like `\thepage` and `\numpages` are available.

``` org
#+EXAM_HEADER_LEFT: CS 101
#+EXAM_HEADER_CENTER: Midterm
#+EXAM_HEADER_RIGHT: Spring 2026
#+EXAM_FOOTER_RIGHT: \thepage/\numpages
```

If you only set a header, a default centre footer of `\thepage` is added so the page number stays visible. Override or disable it by setting `org-exam-default-footer-center` (set to `nil` to leave the centre footer empty).

### Images in the header / footer

Place an institutional logo (or any image) into a header or footer slot using the standard Org file link:

``` org
#+EXAM_HEADER_LEFT: [[file:logos/university.png]]
#+EXAM_HEADER_CENTER: Midterm Exam
#+EXAM_HEADER_RIGHT: [[file:logos/department.pdf]] :width 30mm
#+EXAM_FOOTER_LEFT: [[./logos/footer-mark.png]] :height 8mm
```

- The `file:` prefix is optional — `[[path]]` works as well.
- Append `:height SIZE` or `:width SIZE` after the link to override the size (any LaTeX length: `2cm`, `30mm`, `0.8in`, …). When neither is given, `org-exam-header-image-height` (1.5cm by default) is used.
- The path is resolved relative to the directory of the exported `.tex` file, exactly like `\includegraphics{...}`.
- Any value that doesn't start with `[[…]]` is treated as plain text/LaTeX, so the existing text usage is unchanged.

## Point label

The exam class prints the text ` point` / ` points` in the margin next to each question by default. `org-exam` overrides this with the language-neutral ` pts`. Set a different value per-document with `EXAM_POINT_NAME`:

``` org
#+EXAM_POINT_NAME: puntos
```

The keyword value is rendered with one leading space automatically (Org strips leading whitespace from keyword values, so writing ` pts` directly is not possible — use the [variable](#configuration) for that).

# Configuration

All defcustoms live in the `org-exam` group (`M-x customize-group RET org-exam`).

| Variable                            | Default        | What it does                                                                                                                                                                                                              |
| ----------------------------------- | -------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `org-exam-point-name`               | `" pts"`       | Text appended to point values in the margin (`\pointname`). Conventionally starts with a space. Set to `nil` to keep the exam class default (` point` / ` points`).                                                       |
| `org-exam-default-footer-center`    | `"\\thepage"`  | LaTeX expression used for the centre footer when a custom `EXAM_HEADER_*` is set but no `EXAM_FOOTER_*` is. Switching to the `headandfoot` pagestyle hides the bare page number — this restores it. Set to `nil` to skip. |
| `org-exam-header-image-height`      | `"1.5cm"`      | Default `height` for images embedded in header/footer slots when neither `:height` nor `:width` is given on the file link. Any LaTeX length works.                                                                       |

# Keywords reference

Keywords specific to `org-exam` (in addition to the standard `LATEX_CLASS`, `LATEX_HEADER`, etc.):

| Keyword                                | Effect                                                                  |
| -------------------------------------- | ----------------------------------------------------------------------- |
| `#+EXAM_HEADER_LEFT`/`_CENTER`/`_RIGHT` | Running page header. Plain text, LaTeX, or an Org file link.            |
| `#+EXAM_FOOTER_LEFT`/`_CENTER`/`_RIGHT` | Running page footer. Same value rules as the header.                    |
| `#+EXAM_POINT_NAME`                    | Per-document override of `org-exam-point-name`.                          |

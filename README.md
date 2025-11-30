# org-exam
package for emacs to export exam document class from a org file

# Installation

## Manual installation

1. Clone or download the repository:

``` sh
git clone https://github.com/koprotk/org-exam.git
```

2. Add the folder to your `load-path` and require it: 

``` emacs-lisp
(add-to-list 'load-path "~/path/to/org-exam")
(require 'ox-exam)
```

That's all. The exporter is now available.

## Installation with use-package
In your personal configuration put:
``` emacs-lisp
(use-package org-exam
  :stright (org-exam :host github :repo "koprotk/org-exam")
  :defer t)
```

## Doom Emacs
1. On you `package.el` put:
``` emacs-lisp
(package! org-exam
  :recipe (:host github :repo "koprotk/org-exam"))
```

2. After editing `package.el`
``` sh
doom sync
```

# Usage

1. Your org file must have the following as header:

``` org
#+LATEX_CLASS: exam
```

2. To export, use the following shortcuts:
 - If you want to export the latex document: `C-c C-e l l`
 - If you want to get the latex and pdf: `C-c C-e l o`

# Features supported
## questions (of course)
Just use `*` every is a questions
``` org
* 
```

## Parts
For parts use `**` but depends to have a question before.
``` org
*
**
```

## Subparts
Every part can be divided in subparts for every subpart just use `***`. And a part should be before:
``` org
*
**
***
```

## Points
The points can be set it to every questions, part, and subpart, can be used with `[$ pts]` or as a property of the element 

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
The choices as treated as lists with `-` as marker example, the correct one can be marked with `@correct`

``` org
* Question [1 pts]
- choice 1
- choice 2
- choice 3
- @correct choice 4
```

The choices are printed as A., B., C., etc, for other markers of choices can be set it in the or header.

The correct choices are printed bold only if you have this in the header:

``` org
#+LATEX_CLASS_OPTIONS: answers
```


## Checkboxes
Works similar as choices the only differences that use `+` as list marker.


## Solutions
If you want to have solutions in your exam you just use:

``` org
* Question:
:solution:
here goes your answer of the questions
:end:
```

The solution box is printed only if you have the header:
``` org
#+LATEX_CLASS_OPTIONS: answers
```

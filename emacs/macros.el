;;;; Registers

(set-register ?s '("#+begin_src"
                   "#+end_src"))

(set-register ?q '("#+begin_quote"
                   "#+end_quote"))

(set-register ?o '(
                   "#+TITLE: "
                   "#+DATE: "
                   "#+AUTHOR: "))

(set-register ?l '(
                   "#ifndef LOCK_"
                   "#  define LOCK_"
                   ""
                   ""
                   ""
                   "#endif"))

(set-register ?t '(
                   "struct {"
                   "};"
                   ""
                   "typedef struct ;"))

;;;; Kmacros

(defalias 'mh/copy-thing-at-point
   (kmacro "M-s M-. RET C-@ C-r C-r RET M-w"))

(defalias 'mh/isearch-region
   (kmacro "M-w C-s C-y RET"))

(defalias 'mh/mark-thing-at-point
   (kmacro "M-s M-. RET C-@ C-r C-r RET"))

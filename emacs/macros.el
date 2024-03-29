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

(fset 'mh/copy-thing-at-point
      (kmacro-lambda-form [?\C-\[ ?s ?\C-\[ ?. ?\C-@ ?\C-r ?\C-r ?\C-m ?\C-\[ ?w] 0 "%d"))

(fset 'mh/isearch-region
   (kmacro-lambda-form [?\C-\[ ?w ?\C-s ?\C-y ?\C-m] 0 "%d"))

(fset 'mh/mark-thing-at-point
      (kmacro-lambda-form [?\C-\[ ?s ?\C-\[ ?. ?\C-@ ?\C-r ?\C-r ?\C-m] 0 "%d"))

(fset 'mh/commit
   (kmacro-lambda-form [?\C-s ?d ?i ?f ?f ?\C-r ?\C-m ?\C-@ ?\C-\[ ?> ?\C-\[ ?w ?\C-x ?3 ?\C-x ?\C-o ?\C-x ?b ?d ?i ?f ?f ?\C-j ?\C-y ?\C-\[ ?x ?d ?i ?f ?f ?- ?m ?o ?d ?e ?\C-m ?\C-x ?\C-\[ ?\C-m ?\C-\[ ?\C-e ?\C-\[ ?< ?\C-x ?\C-o ?\C-\[ ?< ?\C-o ?\C-@ ?\C-n ?\C-x ?n ?n ?\C-p ?\C-@ ?\C-@ ?\C-\[ ?x ?a ?u ?t ?o ?- ?f ?i ?l ?l ?- ?m ?o ?d ?e ?\C-m] 0 "%d"))

#!/usr/bin/clisp


(load "~/opt/lisp/lisp-unit.lisp")
(use-package :lisp-unit)

(load "./parse-indents.lisp")


(defun my-command-line ()
  (or 
   #+CLISP *args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(let ((args (my-command-line)))
  (if args
      (dolist (filename args)
        (with-open-file (stream filename)
          (process-stream stream)))
      (process-stream *standard-input*)))

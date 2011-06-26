#!/usr/bin/clisp

(load "./lisp-unit.lisp")
(use-package :lisp-unit)

(load "./parse-indents.lisp")


(lisp-unit:define-test test-split-list
    (lisp-unit:assert-equal
     '(NIL NIL)
     (split-list (lambda (x) (<= x 2)) ()))
    (lisp-unit:assert-equal
     '((3 4 3) (2 1 2 3 2 1))
     (split-list (lambda (x) (<= x 2)) (list 3 4 3 2 1 2 3 2 1)))
    (lisp-unit:assert-equal
     '(NIL (2 3 4 3 2 1 2 3 2 1))
     (split-list (lambda (x) (<= x 2)) (list 2 3 4 3 2 1 2 3 2 1)))
    (lisp-unit:assert-equal
     '((3 4 3 4 5 6 7) NIL)
     (split-list (lambda (x) (<= x 2)) (list 3 4 3 4 5 6 7))))

(defun parse-text-lines (lines)
  (mapcar 'parse-text-line lines))
          
(lisp-unit:define-test test-build-nodes
  (let ((nodes (build-nodes (parse-text-lines nil))))
    (lisp-unit:assert-equal NIL nodes))
  (let ((node (first (build-nodes (parse-text-lines
                                   '("    parent1"
                                     "        child1")))))
        (expected '(:children ((:content "child1")) :content "parent1")))
    (lisp-unit:assert-equal expected node))
  (let ((node (first (build-nodes (parse-text-lines
                                   '("    parent1"
                                     "        child1"
                                     "        child2")))))
        (expected '(:children ((:content "child1")(:content "child2")) :content "parent1")))
    (lisp-unit:assert-equal expected node))
  (let ((nodes (build-nodes (parse-text-lines
                             '("    parent1"
                               "        child1"
                               "    parent2"))))
        (expected '((:children ((:content "child1")) :content "parent1")(:content "parent2"))))
    (lisp-unit:assert-equal expected nodes))
  (let ((nodes (build-nodes (parse-text-lines
                             '("    parent1"
                               "        child1"
                               "        child2"
                               "            grandchild1"
                               "        child3"
                               "    parent2"
                               "    parent3"
                               "        child4"))))
        (expected '((:children ((:content "child1")
                                (:children ((:content "grandchild1")) :content "child2")
                                (:content "child3")) :content "parent1")
                    (:content "parent2")
                    (:children ((:content "child4")) :content "parent3"))))
    (lisp-unit:assert-equal expected nodes))
 )


(run-tests test-split-list test-build-nodes)


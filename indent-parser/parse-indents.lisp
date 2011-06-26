#!/usr/bin/clisp

(defun parse-text-line (line)
  ;; converts "    text" to (:content "text" :indent 4)
  (let ((content (string-left-trim '(#\Space #\Tab) line)))
    (list :content content :indent (- (length line) (length content)))))

(defun split-list (criterion data-list)
  ;; break a list into two lists; the second begins with the first element that matches the criterion.
  (if data-list
      (if (funcall criterion (first data-list))
          (list () data-list)
          (let ((me (pop data-list)))
            (let ((chunks (split-list criterion data-list)))
              (push me (first chunks))
              chunks)))
      (list () ())))

(defun build-nodes (lines)
  (if lines
      (let* ((me (pop lines))
             ;; split off our children from the rest of the lines.
             ;; the first line with an indent <= ours is a sibling, not a child
             (chunks (split-list (lambda (x) (<= (getf x :indent) (getf me :indent))) lines)))
        (let ((children (build-nodes (pop chunks)))
              (siblings (build-nodes (pop chunks))))
          (let ((node (list :content (getf me :content))))
            (if children (setf (getf node :children) children))
            (push node siblings))))
      nil))

;--------------------------------------------------------------------------

(defun process-stream (stream)
  (do ((text-lines nil)
       (text (read-line stream nil) (read-line stream nil)))
      ((equal text nil) (print-tree (build-nodes (mapcar 'parse-text-line (reverse text-lines)))))
    (push text text-lines)))

(defun print-tree (nodes &optional (indent 0))
  (dolist (node nodes)
    ;; e.g. if indent=4, this formats the content with "~4t~a~%"; if indent=0, the format string is just "~a~%"
    ;; format syntax ref: http://www.gigamonkeys.com/book/a-few-format-recipes.html
    (format t (format nil "~[~:;~:*~~~at~]~~a~~%" indent) (getf node :content))
    (print-tree (getf node :children) (+ 4 indent)))
  nodes)

;--------------------------------------------------------------------------

;;(defun my-command-line ()
;;  (or 
;;   #+CLISP *args*
;;   #+LISPWORKS system:*line-arguments-list*
;;   #+CMU extensions:*command-line-words*
;;   nil))
;;
;;(let ((args (my-command-line)))
;;  (if args
;;      (dolist (filename args)
;;        (with-open-file (stream filename)
;;          (process-stream stream)))
;;      (process-stream *standard-input*)))


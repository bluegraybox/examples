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
            ;; (format t "~a~%" (getf node :content))
            (push node siblings))))
      nil))

;;--------------------------------------------------------------------------

(defun show-indented (nodes &optional (indent 0))
  ;; print out the contents of a tree of nodes, indented appropriately
  (dolist (node nodes)
    ;; e.g. if indent=4, this formats the content with "~4t~a~%"; if indent=0, the format string is just "~a~%"
    ;; format syntax ref: http://www.gigamonkeys.com/book/a-few-format-recipes.html
    (format t (format nil "~[~:;~:*~~~at~]~~a~~%" indent) (getf node :content))
    (show-indented (getf node :children) (+ 4 indent)))
  t)

(defun show-parents (nodes &optional (parents nil))
  ;; print out the leaves of a tree with their parent path (root::parent::child::leaf)
  (dolist (node nodes)
    (let* ((children (getf node :children))
           (content (getf node :content))
           (text (if parents (format nil "~a::~a" parents content) content)))
      (if children
          (show-parents children text)
          (format t "~a~%" text))))
  t)

(defun process-stream (stream show-func)
  ;; build a tree from an input stream of indented text
  (do ((text-lines nil)
       (text (read-line stream nil) (read-line stream nil)))
      ((equal text nil)
       (funcall show-func (build-nodes (mapcar 'parse-text-line (reverse text-lines)))))
    (push text text-lines)))

;;--------------------------------------------------------------------------


(defun my-command-line ()
  (or 
   #+CLISP *args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(let ((args (my-command-line))
      (sig-char #\*))
  (if args
      (let ((show-func (if (equal sig-char (elt (first args) 0))
                           (intern (string-upcase (remove sig-char (pop args) :count 1)))
                           'show-parents)))
        (dolist (filename args)
          (if (equal "-" filename)
              (process-stream *standard-input* show-func)
              (with-open-file (stream filename)
                (process-stream stream show-func)))))))

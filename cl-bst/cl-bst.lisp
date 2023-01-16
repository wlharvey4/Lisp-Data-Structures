;;; cl-bst.lisp - Binary Search Tree procedures in Common Lisp
;;; Time-stamp: <2023-01-16 01:46:47 minilolh3>

;;; Author: LOLH
;;; Created: 2023-01-14
;;; Version: 0.1.3

;;; Commentary

;;; Code:

(defpackage :lolh.utils
  (:use :cl)
  (:export :*cl-bst*
	   :*cl-bst-eqs*
	   :*cl-bst-lt*
	   :*cl-bst-gt*
	   :*cl-bst-eq*
	   :make-bst-node
	   :cl-bst-set-cmp-funcs
	   :bst-insert!-node
	   :bst-delete!node
	   :bst-delete-node
	   :bst-inorder-traversal
	   :bst-preorder-traversal
	   :bst-postorder-traversal
	   :bst-find-node
	   :bst-min
	   :bst-max
	   :bst-size
	   :bst-height))

(in-package :lolh.utils)

(defstruct bst-node
  left
  data
  right)

(defparameter *cl-bst-eqs* ()
  "Add nodes that are equal here; later use this to delete them.")

(defparameter *cl-bst-data*
  '(50 25 75 10 30 60 80 80 5 12 28 85 29 29)
  "This is some sample data for testing.")

;; Default comparison functions; these are for integer data..
(defparameter *cl-bst-lt* #'<)
(defparameter *cl-bst-gt* #'>)
(defparameter *cl-bst-eq* #'=)

(defparameter *cl-bst* (make-bst-node)
  "*cl-bst* is the root node and starts out empty.")

(defun cl-bst-set-cmp-funcs (&key lt gt eq)
  "Use this procedure to set comparison functions for different types
of data structures."
  (setf *cl-bst-lt* lt
	*cl-bst-gt* gt
	*cl-bst-eq* eq))

(defun empty-bst-node (bst)
  (and
   (null (bst-node-left bst))
   (null (bst-node-right bst))
   (null (bst-node-data bst))))

(defun bst-insert!-node (data bst)
  "This is a destructive procedure.  It cannot modify an initial
nil value, however, so start with a non-nil initial bst-node."
  (cond ((empty-bst-node bst)
	 (setf (bst-node-data bst) data))
	((funcall *cl-bst-lt* data (bst-node-data bst))
	 (setf (bst-node-left bst)
	       (bst-insert!-node data
				 (if (bst-node-left bst)
				     (bst-node-left bst)
				     (make-bst-node)))))
	((funcall *cl-bst-gt* data (bst-node-data bst))
	 (setf (bst-node-right bst)
	       (bst-insert!-node data
				 (if (bst-node-right bst)
				     (bst-node-right bst)
				     (make-bst-node)))))
	(t (setf *cl-bst-eqs* (cons (bst-node-data bst) *cl-bst-eqs*))
	   (break)))
  bst)

(defun bst-insert-nodes (data-list bst)
  "This procedure inserts the test data into a BST."
  (dolist (data data-list bst)
    (bst-insert!-node data bst)))

(defun bst-delete!-node (data bst)
  "This is a destructive procedure."
  (if (null bst)
      (format t "The data item ~D is not in this tree.~%" data)
      (progn
	(cond ((funcall *cl-bst-lt* data (bst-node-data bst))
	       (setf (bst-node-left bst)
		(bst-delete!-node data (bst-node-left bst))))
	      ((funcall *cl-bst-gt* data (bst-node-data bst))
	       (setf (bst-node-right bst)
		(bst-delete!-node data (bst-node-right bst))))
	      (t ; this is the node to delete
	       (if (and (bst-node-left bst) (bst-node-right bst))
		   ;; Two children exist so
		   ;; replace this data with the largest data in they
		   ;; left subtree
		   ;; (or the smallest data in the right subtree)
		   ;; and recursively delete that max (or min) node.
		   (let ((temp (bst-max (bst-node-left bst))))
		     (setf (bst-node-data bst) (bst-node-data temp))
		     (setf (bst-node-left bst)
			   (bst-delete!-node
			    (bst-node-data temp) (bst-node-left bst))))
		   ;; one or no child exists
		   (cond ((null (bst-node-left bst))
			  (setf bst (bst-node-right bst)))
			 ((null (bst-node-right bst))
			  (setf bst (bst-node-left bst)))
			 (t (setf bst nil))))))
	bst)))

(defun bst-delete-node (data bst)
  "This is a non-destructive procedure that deletes the node with the
specified data."
  (when (null bst)
    (format t "The data item ~D is not in this tree.~%" data)
    (return-from bst-delete-node))
  ;; Search for the node with the data
  (cond ((funcall *cl-bst-lt* data (bst-node-data bst))
	 ;; keep searching left
	 (make-bst-node
	  :data (bst-node-data bst)
	  :left (bst-delete-node data (bst-node-left bst))
	  :right (bst-node-right bst)))
	((funcall *cl-bst-gt* data (bst-node-data bst))
	 ;; keep searching right
	 (make-bst-node
	  :data (bst-node-data bst)
	  :right (bst-delete-node data (bst-node-right bst))
	  :left (bst-node-left bst)))
	(t ;Found the node to delete
	 (if (and (bst-node-left bst) (bst-node-right bst))
	     ;; Two children exist so
	     ;; replace this data with the largest data in the left subtree
	     ;; (or the smallest data in the right subtree)
	     ;; and recursively delete that max (or min) node.
	     (let* ((max-node (bst-max (bst-node-left bst)))
		    (max-data (bst-node-data max-node)))
	       (make-bst-node :data max-data :right (bst-node-right bst)
			      :left (bst-delete-node max-data
						     (bst-node-left bst))))
	     ;; one or no child exists
	     (cond ((null (bst-node-left bst))
		    (bst-node-right bst))
		   ((null (bst-node-right bst))
		    (bst-node-left bst))
		   (t nil))))))

(defun bst-inorder-traversal (bst)
  (when bst
    (bst-inorder-traversal (bst-node-left bst))
    (print (bst-node-data bst))
    (bst-inorder-traversal (bst-node-right bst)))
  t)

(defun bst-preorder-traversal (bst)
  (when bst
    (print (bst-node-data bst))
    (bst-preorder-traversal (bst-node-left bst))
    (bst-preorder-traversal (bst-node-right bst))))

(defun bst-postorder-traversal (bst)
  (when bst
    (bst-postorder-traversal (bst-node-right bst))
    (print (bst-node-data bst))
    (bst-postorder-traversal (bst-node-left bst)))
  t)

(defun bst-find-node (data bst)
  (when bst
    (let ((d (bst-node-data bst))
	  (l (bst-node-left bst))
	  (r (bst-node-right bst)))
      (cond ((funcall *cl-bst-lt* data d)(bst-find-node data l))
	    ((funcall *cl-bst-gt* data d)(bst-find-node data r))
	    (t bst)))))

(defun bst-min (bst)
  (when bst
    (if (null (bst-node-left bst))
	bst
	(bst-min (bst-node-left bst)))))

(defun bst-max (bst)
  (when bst
    (if (null (bst-node-right bst))
	bst
	(bst-max (bst-node-right bst)))))

(defun bst-size (bst)
  (if (null bst)
      0
      (+
       (bst-size (bst-node-left bst))
       1
       (bst-size (bst-node-right bst)))))

(defun bst-height (bst)
  "The height of a node in a binary tree is the largest number of edges
in a path from a leaf node to a target node. If the target node
doesn’t have any other nodes connected to it, the height of that node
would be \mathsf{0}.  That is, the height of the empty tree is
zero. The height of a binary tree is the height of the root node in
the whole binary tree. In other words, the height of a binary tree is
equal to the largest number of edges from the root to the most distant
leaf node."
  (if (empty-bst-node bst)
      0
      (let ((left (bst-height (if (bst-node-left bst)
				  (bst-node-left bst)
				  (make-bst-node))))
	    (right (bst-height (if (bst-node-right bst)
				   (bst-node-right bst)
				   (make-bst-node)))))
	(1+ (max left right)))))

;; (defun bst-node-depth (bst target)
;;   "The depth of a node in a binary tree is the total number of edges from
;; the root node to the target node. Similarly, the depth of a binary
;; tree is the total number of edges from the root node to the most
;; distant leaf node."
;;   nil)

;; (lolh.utils::bst-insert-nodes lolh.utils::*cl-bst-data* lolh.utils:*cl-bst*)


;;; End cl-bst.lisp

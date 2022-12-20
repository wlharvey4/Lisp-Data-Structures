;;; -*- mode: Lisp; -*-

(defparameter *bst* ())
(defparameter *bst-data*
  '(50 25 75 10 30 60 80 5 12 28 85 29))

(defstruct bst-node
  left data right)

(defun bst-insert!-node (data bst)
  "This is a destructive procedure.  It cannot modify an initial
nil value, however, so start with a non-nil initial bst-node."
  (cond ((null bst)
	 (setf bst (make-bst-node :data data)))
	((< data (bst-node-data bst))
	 (setf (bst-node-left bst)
	       (bst-insert!-node data (bst-node-left bst))))
	((> data (bst-node-data bst))
	 (setf (bst-node-right bst)
	       (bst-insert!-node data (bst-node-right bst)))))
  bst)

(defun bst-insert-nodes (data-list bst)
  (dolist (data data-list bst)
    (setf bst (bst-insert!-node data bst))))

(defun bst-delete!-node (data bst)
  "This is a destructive procedure."
  (if (null bst)
      (format t "The data item ~D is not in this tree.~%" data)
      (progn
	(cond ((< data (bst-node-data bst))
	       (setf (bst-node-left bst)
		(bst-delete!-node data (bst-node-left bst))))
	      ((> data (bst-node-data bst))
	       (setf (bst-node-right bst)
		(bst-delete!-node data (bst-node-right bst))))
	      (t ; this is the node to delete
	       (if (and (bst-node-left bst) (bst-node-right bst))
		   ;; Two children exist so
		   ;; replace this data with the largest data in the
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
  (cond ((< data (bst-node-data bst))
	 ;; keep searching left
	 (make-bst-node
	  :data (bst-node-data bst)
	  :left (bst-delete-node data (bst-node-left bst))
	  :right (bst-node-right bst)))
	((> data (bst-node-data bst))
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


;; (defun bst-delete-node (data bst)
;;   "Procedure to delete a node:
;; 1. Find the node to be deleted, and retain a reference to the
;;    parent node as a left or right.
;; 2. If the node to be deleted is a leaf node (no children), set
;;    parent left or right node to null.
;; 3. If the node to be deleted has only one child, set the parent
;;    left or right node to this child; null the node to be deleted.
;; 4. IF the node to be deleted has two children, find and use the
;;    highest subtree.
;;    a. If the highest subtree is on the left, find the max value
;;       and replace the data of the node to be deleted with it; then
;;       recursively delete that max value.
;;    b. If the highest subtree is on the right, find the min value
;;       and replace the date of the node to be deleted with it; then
;;       recursively delete the min value."
;;   (labels ((find-node-with-parent (d tr)
;;	     "Returns a list, the first value being the parent and the
;;            second value being the node to be deleted."
;; 	     (when (null tr)
;; 	       (format t "The data item ~D was not found in the search
;;              tree.~%" d)
;; 	       (return-from bst-delete-node))
;; 	     (let* ((tr-l (bst-node-left tr))
;; 		    (tr-r (bst-node-right tr))
;; 		    (d-l (and tr-l (bst-node-data tr-l)))
;; 		    (d-r (and tr-r (bst-node-data tr-r)))
;; 		    (d-tr (bst-node-data tr)))
;; 	       (cond ((eql d d-l) (values tr tr-l "left"))
;; 		     ((eql d d-r) (values tr tr-r "right"))
;; 		     (t (find-node-with-parent d
;; 					       (if (< d d-tr)
;; 						   tr-l
;; 						   tr-r)))))))
;;     (multiple-value-bind (parent node side) (find-node-with-parent data bst)
;;       (let ((num-children (+ (if (bst-node-left node)
;; 				 1 0)
;; 			     (if (bst-node-right node)
;; 				 1 0))))
;; 	(case num-children
;; 	  (0
;; 	   (case side
;; 	     ("left" (setf (bst-node-left parent) nil) *bst*)
;; 	     ("right"(setf (bst-node-right parent) nil) *bst*)))
;; 	  (1
;; 	   (case side
;; 	     ("left"
;; 	      (setf (bst-node-left parent) (or (bst-node-left node)
;; 					       (bst-node-right node))))
;; 	     ("right"
;; 	      (setf (bst-node-right parent) (or (bst-node-left node)
;; 						(bst-node-right node))))))
;; 	  (2
;; 	   (let ((l-height (bst-height (bst-node-left node)))
;; 		 (r-height (bst-height (bst-node-right node))))
;; 	     (if (> l-height r-height)
;; 		 (progn
;; 		   (let* ((max-child (bst-max (bst-node-left node)))
;; 			  (max-child-data (bst-node-data max-child)))
;; 		     (bst-delete-node max-child-data node)
;; 		     (setf (bst-node-data node) max-child-data)))
;; 		 (progn
;; 		   (let* ((min-child (bst-min (bst-node-right node)))
;; 			  (min-child-data (bst-node-data min-child)))
;; 		     (bst-delete-node min-child-data node)
;; 		     (setf (bst-node-data node) min-child-data))))))))))
;;   bst)

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
      (cond ((< data d)(bst-find-node data l))
	    ((> data d)(bst-find-node data r))
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
  (let ((left 0)
	(right 0))
    (if (null bst)
	0
	(progn (setf left (bst-height (bst-node-left bst)))
	       (setf right (bst-height (bst-node-right bst)))
	       (if (> left right)
		   (1+ left)
		   (1+ right))))))

(setf *bst* (bst-insert-nodes *bst-data* *bst*))

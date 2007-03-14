
;;; pensando sobre a estrutura de grafo

(defstruct node contents childs)

(defvar *nodes* (make-hash-table))

(defun def-node (name &optional lst)
  (setf (gethash name *nodes*)
	(make-node :contents name
		   :childs lst)))

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (if (null n)
	name
	(list (node-contents n) 
	      (mapcar #'run-node (node-childs n))))))


; criei a estrutura
(def-node 'a '(b c))
(def-node 'b '(d e))

; busca em largura
(run-node 'a)

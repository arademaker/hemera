
;;; grafo usando cl-grap

; (require 'asdf)
(asdf:operate 'asdf:load-op :cl-graph)

(let ((g (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed)))
  (loop for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)) do
       (cl-graph:add-edge-between-vertexes g a b))
  (cl-graph:graph->dot g nil))


;;; pensando sobre o problema

(let (graph nil)
  (defun make-graph )
  (defun graph-add-edge)
  (defun graph-add-node))


(defun parse-formulae (formulae) )


(defun make-proof (f) 
  (make-graph (parse-formulae f))





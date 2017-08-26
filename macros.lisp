;;;; USEFUL MACROS SHOULD GO HERE

;;; aif macro taken directly from Paul Graham's "On Lisp"
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
	 (if it ,then-form ,else-form)))
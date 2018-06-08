(defun unintern-shadowing-symbols (package)
  (let ((syms (package-shadowing-symbols package)))
    (dolist (sym syms) (unintern sym package))
    (format t "uninterned ~D shadowing symbols ~%"
            (length syms))))

;; C-- C-c x

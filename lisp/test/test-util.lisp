(defun fiasco-clear-tests (&key (package *package*))
  (loop
     with suite = (fiasco::find-suite-for-package package)
     with table = (slot-value suite 'fiasco::children)
     for k being the hash-keys of table
     do (remhash k table)))

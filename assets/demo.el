(defun my/silly-function (xs)
  (let ((stack xs)
        (n 1))
    (concat
     (with-temp-buffer
       (save-excursion
         (while stack
           (insert
            (format "===============
step %d: %s
===============
" n stack))
           (setq stack (cdr stack))
           (cl-incf n)))
       (buffer-string))
     (propertize
      "-*- THE -*-
-*- END -*-"
      'face 'highlight))))

;; Local Variables:
;; eval: (multilign-mode +1)
;; End:

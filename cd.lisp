(defvar *db* nil)

(defun make-cd (title name rating ripped)
  (list :title title :name name :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun print-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Name")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-expr (field value)
  `(equal (getf cd, field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

(defun set-field (row field new-value)
  (setf (getf row field) new-value))

(defun set-fields (row fields)
  (loop while fields
        collecting (set-field row (pop fields) (pop fields))))

(defun update (selector-fn &rest clauses); &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (set-fields row clauses)
                ;(if title (setf (getf row :title) title))
                ;(if artist (setf (getf row :artist) artist))
                ;(if rating (setf (getf row :rating) rating))
                ;(if ripped-p (setf (getf row :ripped) ripped)))
                )
              row) *db*)))

(defun delete-rows (selector-fn)
     (setf *db* (remove-if selector-fn *db*)))

(add-record (make-cd "My Song #1" "Me" 10 t))
(add-record (make-cd "My Song #2" "Me" 10 t))
(add-record (make-cd "Your Song #1" "You" 2 t))
(add-record (make-cd "Your Song #2" "You" 2 t))

;(add-record (prompt-for-cd))
(print-db)
(save-db "./test.db")

(print (select (where :title "My Song #1" :ripped t)))

(update (where :title "My Song #1" :ripped t) :rating 11)
(print-db)

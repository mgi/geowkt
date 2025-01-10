(in-package :geowkt-update)

(setf *read-default-float-format* 'double-float)

(defstruct token type value position)

(defun tokenp (token type &optional value)
  (if value
      (and (eql (token-type token) type)
           (eql (token-value token) value))
      (eql (token-type token) type)))

(define-condition wkt-parse-error (simple-error) ())

(defparameter *whitespace-chars*
  (concatenate 'string (list #\space #\tab #.(code-char 11) #\page (code-char #xa0))))

(defun tokenize (stream)
  (let ((position 0))
    (labels ((token (type value)
               (make-token :type type :value value :position position))
             (peek ()
               (peek-char nil stream nil))
             (next (&optional eof-error)
               (let ((ch (read-char stream eof-error)))
                 (when ch
                   (incf position)
                   ch)))
             (skip-whitespace ()
               (loop for ch = (peek)
                     while (and ch (find ch *whitespace-chars*))
                     do (next)))
             (number-char-p (ch) (or (digit-char-p ch) (find ch ".e-+")))
             (read-number ()
               (let ((numstr (with-output-to-string (*standard-output*)
                               (loop for ch = (peek) do
                                 (if (and ch (number-char-p ch))
                                     (write-char (next))
                                     (return))))))
                 (token :number (parse-number:parse-number numstr))))
             (word-char-p (ch) (alphanumericp ch))
             (read-word ()
               (let ((word (with-output-to-string (*standard-output*)
                             (loop for ch = (peek)
                                   do (if (and ch (word-char-p ch))
                                          (write-char (next))
                                          (return))))))
                 (token :word word)))
             (read-string ()
               (let ((str (with-output-to-string (*standard-output*)
                            (loop for ch = (next)
                                  until (char= ch #\")
                                  do (when ch
                                       (write-char ch))))))
                 (token :string str)))
             (next-token ()
               (skip-whitespace)
               (let ((next (peek)))
                 (cond ((null next) (token :eof "EOF"))
                       ((char= next #\") (next) (read-string))
                       ((char= next #\[) (token :block-start (next)))
                       ((char= next #\]) (token :block-end (next)))
                       ((char= next #\,) (token :comma (next)))
                       ((number-char-p next) (read-number))
                       ((word-char-p next) (read-word))
                       (t (error 'wkt-parse-error :format-control "Unexpected character '~a' at ~d."
                                                  :format-arguments (list next position)))))))
      #'next-token)))

(defun %parse (stream)
  (let* ((input (tokenize stream))
         (token (funcall input))
         peeked words)
    (labels ((peek () (or peeked (setf peeked (funcall input))))
             (next ()
               (if peeked
                   (setf token peeked peeked nil)
                   (setf token (funcall input))))
             (block* ()
               (loop with result = (list (pop words))
                     until (or (tokenp token :eof)
                               (tokenp token :block-end))
                     do (let ((s (statement)))
                          (when s (push s result)))
                     finally (progn
                               (when words (push (pop words) result))
                               (return (reverse result)))))
             (statement ()
               (prog1 (case (token-type token)
                        (:comma (when words (pop words)))
                        (:word (push (intern (token-value token) :keyword) words) (values))
                        ((:number :string) (token-value token))
                        (:block-start (next) (block*)))
                 (next))))
      (loop until (tokenp token :eof)
            when (statement)
              nconc it))))

(defun %get-tokens (string)
  "Debugging helper"
  (with-input-from-string (stream string)
    (let ((f (tokenize stream)))
      (loop for token = (funcall f)
            until (tokenp token :eof)
            collect token))))

(defun parse (string)
  (with-input-from-string (stream string)
    (%parse stream)))

(defun get-online (epsg-code)
  (multiple-value-bind (content code)
      (drakma:http-request (format nil "https://spatialreference.org/ref/epsg/~d/ogcwkt/" epsg-code))
    (when (= code 200)
      content)))

(defun update-dbs (&optional (codes *epsgs*))
  "Update both databases in \"db.lisp\" and \"name-db.lisp\"."
  (let ((name-db (make-hash-table :test 'equal)))
    (with-open-file (out #p"db.lisp" :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (when (zerop (file-position out))
        (write '(in-package :geowkt) :stream out)
        (terpri out))
      (loop for code in codes
            do (handler-case
                   (let ((response (get-online code)))
                     (when response
                       (let ((entry (parse response)))
                         ;; Write entry for the code DB.
                         (write `(setf (gethash ,code *db*) ',entry) :stream out)
                         (terpri out)
                         (finish-output out)
                         ;; The name based DB is subject to collision.
                         ;; So we first build the hash table and then
                         ;; write it to a file below.
                         (let ((name (and entry (second entry))))
                           (multiple-value-bind (v presentp) (gethash name name-db)
                             (if presentp
                                 ;; Is it a simple entry or a list of
                                 ;; such entries?
                                 (if (member (car v) '(:PROJCS :GEOGCS))
                                     (setf (gethash name name-db) (list entry v))
                                     (setf (gethash name name-db) (push entry v)))
                                 (setf (gethash name name-db) entry))))))
                     ;; Do not load the server too much.
                     (sleep 0.1))
                 (wkt-parse-error ()))))
    ;; Output name database.
    (with-open-file (out #p"name-db.lisp" :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
      (when (zerop (file-position out))
        (write '(in-package :geowkt) :stream out)
        (terpri out))
      (maphash #'(lambda (k v)
                   (write `(setf (gethash ,k *name-db*) ',v) :stream out)
                   (terpri out))
               name-db))))

;;; Helper to keep *epsgs* up to date.  Parse
;;; "https://spatialreference.org/crslist.json" to this end.

(defun get-code-from-entry (entry)
  (let ((a (assoc :code entry)))
    (and a (parse-integer (cdr a)))))

(defun filter-epsg-only (list)
  (remove-if-not
   #'(lambda (x)
       (let ((a (assoc :auth--name x)))
         (and a (string-equal "EPSG" (cdr a)))))
   list))

(defun update-codes-list ()
  (multiple-value-bind (content code)
      (drakma:http-request "https://spatialreference.org/crslist.json")
    (when (= code 200)
      (let* ((parsed (json:decode-json-from-string
                      (sb-ext:octets-to-string content)))
             (codes (mapcar #'get-code-from-entry (filter-epsg-only parsed))))
        (with-open-file (out #p"epsg-codes.lisp" :direction :output
                                                 :if-exists :supersede)
          (write '(in-package :geowkt-update) :stream out)
          (terpri out)
          (write `(defparameter *epsgs* ',codes) :stream out)
          (terpri out))))))

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
              collect it))))

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

(defun update-db (&optional (codes *epsgs*))
  (with-open-file (out #p"db.lisp" :direction :output
                                   :if-exists :append
                                   :if-does-not-exist :create)
    (when (zerop (file-position out))
      (write '(in-package :geowkt) :stream out)
      (terpri out))
    (loop for code in codes
          do (handler-case
                 (let ((response (get-online code)))
                   (when response
                     (write `(setf (gethash ,code *db*) ',(parse response)) :stream out)
                     (terpri out)
                     (finish-output out))
                   (sleep 0.1))
               (wkt-parse-error ())))))

;;; Helper to keep *epsgs* up to date.  XXX The two following routines
;;; do not work anymore now that spatialreference.org is using
;;; javascript to render those pages.
(defun codes-from-page (page)
  "Get defined EPSG codes from spatialreference.org PAGE."
  (let ((regex "/ref/epsg/([0-9]+)"))
    (multiple-value-bind (content code)
        (drakma:http-request (format nil "https://spatialreference.org/ref/epsg/?page=~d" page))
      (when (= code 200)
        (labels ((parse-ref (ref)
                   (multiple-value-bind (match regs) (ppcre:scan-to-strings regex ref)
                     (declare (ignore match))
                     (parse-integer (svref regs 0)))))
          (mapcar #'parse-ref (ppcre:all-matches-as-strings regex content)))))))

(defun update-codes-list ()
  (with-open-file (out #p"epsg-codes.lisp" :direction :output
                                           :if-exists :supersede)
    (write '(in-package :geowkt-update) :stream out)
    (terpri out)
    (let ((codes (loop for page from 1 to 88
                       for codes = (codes-from-page page)
                       append codes)))
      (write `(defparameter *epsgs* ',codes) :stream out)
      (terpri out))))

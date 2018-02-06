;;
;;  babel-stream  -  charset encoding/decoding layer for cl-stream
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :babel-stream)

(defclass babel-input-stream (babel-stream input-stream)
  ((bytes :initform (make-array '(8) :element-type '(unsigned-byte 8))
          :reader stream-bytes
          :type (array (unsigned-byte 8)))
   (bytes-length :initform 0
                 :accessor stream-bytes-length
                 :type fixnum+)))

(defmethod stream-read ((stream babel-input-stream))
  (let* ((underlying-stream (stream-underlying-stream stream))
         (bytes (stream-bytes stream))
         (encoding (stream-external-format stream))
         (mapping (babel::lookup-mapping babel::*string-vector-mappings*
                                         encoding)))
    (loop
       (multiple-value-bind (element state) (stream-read
                                             underlying-stream)
         (case state
           ((nil)
            (setf (aref bytes (stream-bytes-length stream)) element)
            (incf (stream-bytes-length stream))
            (handler-case
                (let ((string (make-string 1)))
                  (when (= 1 (funcall (the function (babel::decoder mapping))
                                      bytes 0 (stream-bytes-length stream)
                                      string 0))
                    (setf (stream-bytes-length stream) 0)
                    (return (values (char string 0) nil))))
              (babel-encodings:end-of-input-in-character ())))
           ((:eof)
            (return (values nil :eof)))
           ((:non-blocking)
            (return (values nil :non-blocking)))
           (otherwise
            (error 'stream-input-error :stream stream)))))))

(defun babel-input-stream (stream &optional (external-format :utf-8))
  (make-instance 'babel-input-stream
                 :external-format external-format
                 :stream stream))

#+test
(let ((s (make-instance 'babel-input-stream
                        :stream (fd-stream:fd-input-stream 0))))
  (read-line s))

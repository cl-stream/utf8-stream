;;
;;  utf8-stream  -  charset encoding/decoding layer for cl-stream
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

(in-package :utf8-stream)

(defclass utf8-output-stream (utf8-stream output-stream)
  ((bytes :initform (make-array '(8) :element-type '(unsigned-byte 8))
          :reader stream-bytes
          :type (array (unsigned-byte 8)))
   (bytes-length :initform 0
                 :accessor stream-bytes-length
                 :type fixnum+)))

(defmethod stream-write ((stream utf8-output-stream)
                         (element character))
  (stream-write (stream-underlying-stream stream) element))

(defun decode (stream bytes)
  (let* ((mapping (mapping stream))
         (string (make-string 1)))
    (when (= 1 (funcall (the function (babel::decoder mapping))
                        bytes 0 (stream-bytes-length stream)
                        string 0))
      (setf (stream-bytes-length stream) 0)
      (stream-write (stream-underlying-stream stream)
                    (char string 0)))))
        
(defmethod stream-write ((stream utf8-output-stream)
                         (element integer))
  (assert (typep element '(unsigned-byte 8)))
  (let ((bytes (stream-bytes stream)))
    (setf (aref bytes (stream-bytes-length stream)) element)
    (incf (stream-bytes-length stream))
    (handler-case
        (decode stream bytes)
      (babel-encodings:end-of-input-in-character ()))))

(defun utf8-output-stream (stream &optional (format :utf-8))
  (make-instance 'utf8-output-stream
                 :format format
                 :stream stream))

#+test
(let ((s (make-instance 'utf8-output-stream
                        :stream (fd-stream:fd-output-stream 1))))
  (write-sequence s "Hello, world ! ÉÀÖÛŸ")
  (flush s))

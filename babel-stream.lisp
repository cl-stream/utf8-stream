;;
;;  babel-stream  -  charset encoding/decoding layer for cl-stream
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
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

(defclass babel-stream (stream)
  ((underlying-stream :initarg :stream
                      :accessor stream-underlying-stream
                      :type stream)
   (external-format :initarg :external-format
                    :initform :utf-8
                    :accessor stream-external-format
                    :type symbol)))

(defmethod initialize-instance :after ((stream babel-stream)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (assert (equal '(unsigned-byte 8)
                 (stream-element-type (stream-underlying-stream stream)))))

(defmethod stream-element-type ((stream babel-stream))
  'character)

(defclass babel-input-stream (babel-stream input-stream)
  ((bytes :initform (make-array '(8) :element-type '(unsigned-byte 8))
          :reader stream-bytes
          :type (array (unsigned-byte 8)))
   (bytes-length :initform 0
                 :accessor stream-bytes-length
                 :type fixnum+)))

(defmethod read ((stream babel-input-stream))
  (let* ((underlying-stream (stream-underlying-stream stream))
         (bytes (stream-bytes stream))
         (encoding (stream-external-format stream))
         (mapping (babel::lookup-mapping babel::*string-vector-mappings*
                                         encoding)))
    (loop
       (multiple-value-bind (element state) (read underlying-stream)
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

(defclass babel-output-stream (babel-stream output-stream)
  ())

(defmethod write ((stream babel-output-stream) (element fixnum))
  (assert (typep element '(unsigned-byte 8)))
  (write (stream-underlying-stream stream) element))

(defmethod write ((stream babel-output-stream) (element character))
  (let* ((encoding (stream-external-format stream))
         (mapping (babel::lookup-mapping babel::*string-vector-mappings*
                                         encoding))
         (string (make-string 1 :initial-element element))
         (bytes (make-array '(8) :element-type '(unsigned-byte 8)))
         (length (funcall (the function (babel::encoder mapping))
                          string 0 1 bytes 0)))
    (write-sequence (stream-underlying-stream stream)
                    bytes :end length)))

(defmethod flush ((stream babel-output-stream))
  (flush (stream-underlying-stream stream)))

(defmethod stream-flush-output-buffer ((stream babel-output-stream))
  (stream-flush-output-buffer (stream-underlying-stream stream)))

(defun babel-output-stream (stream &optional (external-format :utf-8))
  (make-instance 'babel-output-stream
                 :external-format external-format
                 :stream stream))

#+test
(let ((s (make-instance 'babel-output-stream
                        :stream (fd-stream:fd-output-stream 1))))
  (write-sequence s "Hello, world ! ÉÀÖÛŸ")
  (flush s))

(defclass babel-io-stream (babel-input-stream babel-output-stream)
  ())

(defun babel-io-stream (stream &optional (external-format :utf-8))
  (make-instance 'babel-io-stream
                 :external-format external-format
                 :stream stream))

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

(defclass utf8-input-stream (utf8-stream buffered-input-stream)
  ()
  (:default-initargs :input-buffer-size 8))

(defun encode (stream character)
  (let* ((mapping (mapping stream))
         (string (make-string 1 :initial-element character))
         (bytes (stream-input-buffer stream))
         (length (funcall (the function (babel::encoder mapping))
                          string 0 1 bytes 0)))
    (setf (stream-input-index stream) 0
          (stream-input-length stream) length))
  nil)

(defmethod stream-fill-input-buffer ((stream utf8-input-stream))
  (multiple-value-bind (element state)
      (stream-read (stream-underlying-stream stream))
    (case state
      ((nil)
       (encode stream element)
       nil)
      ((:eof) :eof)
      ((:non-blocking) :non-blocking)
      (otherwise
       (error 'stream-input-error :stream stream)))))

(defun utf8-input-stream (stream &optional (format :utf-8))
  (make-instance 'utf8-input-stream
                 :format format
                 :stream stream))

#+test
(let ((s (make-instance 'utf8-input-stream
                        :stream (fd-stream:fd-input-stream 0))))
  (read-line s))

(defmethod stream-read-line ((stream utf8-stream))
  (stream-read-until (babel-stream:babel-input-stream stream)
                     #\Newline))

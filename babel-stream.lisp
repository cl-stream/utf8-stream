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
  ())

(defclass babel-output-stream (babel-stream output-stream)
  ())

(defmethod write ((stream babel-output-stream) (element fixnum))
  (assert (typep element '(unsigned-byte 8)))
  (write (stream-underlying-stream stream) element))

(defmethod write ((stream babel-output-stream) (element character))
  (let ((seq (babel:string-to-octets (make-string 1 :initial-element element)
				     :encoding (stream-external-format stream)
				     :use-bom nil)))
    (write-sequence (stream-underlying-stream stream)
		    seq)))

(defmethod flush ((stream babel-output-stream))
  (flush (stream-underlying-stream stream)))

(let ((s (make-instance 'babel-output-stream
			:stream (fd-stream:fd-output-stream 1))))
  (write s #\é)
  (flush s))
